# ======================================================================
# Structure of this file
# ======================================================================
# calc_prevalence() is the only exported function. It calls small internal
# helpers (names start with ".prev_"), one per step:
#
#   1. .prev_validate_args()        check the arguments
#   2. .prev_copy_columns()         private copy of the needed columns
#      .prev_check_year()           check the year variable
#      .prev_restrict_population()  denominator + missing outcome
#      .prev_prepare_weights()      validate / create weights
#   3. .prev_define_periods()       rolling or user-defined periods
#      .prev_stack_periods()        one data set with all periods
#   4. .prev_build_design()         survey design
#      .prev_run_svyby()            estimate prevalence per domain
#      .prev_extract_estimates()    estimates + confidence intervals
#      .prev_get_covariance()       covariance of the estimates
#   5. .prev_run_tests()            pairwise tests   -> $tests
#      .prev_run_trend()            trend + fitted   -> $trend, $fitted
#
# The helpers can be tested on their own with small made-up inputs, e.g.
#   calc_prevalence:::.prev_wald_test(0.02, 0.0001)
# or directly (without ':::') inside tests/testthat of the package.
# ======================================================================

#' Calculate prevalence, confidence intervals, significance tests and trend
#'
#' Calculates survey-weighted prevalence estimates and confidence intervals
#' for a binary outcome over single years, rolling multi-year periods or
#' user-defined groups of years, and (optionally) tests whether the prevalence
#' differs significantly between periods or between subgroups, and whether
#' there is a linear trend over time. Different analysis populations can be
#' specified for different outcomes through the `denominator` variable.
#'
#' @param data A data frame or data.table containing the survey data.
#' @param denominator Character string giving the name of a binary
#'   denominator/eligibility variable. Observations with `denominator == 1`
#'   are included in the analysis population; observations with
#'   `denominator == 0` are excluded. The denominator variable therefore
#'   defines the population to which the prevalence estimate applies.
#' @param year_var Character string giving the name of the variable
#'   containing the calendar year (whole numbers).
#' @param outcome_var Character string giving the name of the binary
#'   outcome variable. The outcome must be coded as `0` and `1`, where
#'   `1` indicates the outcome of interest.
#' @param weight_var Optional character string giving the name of the survey
#'   weight variable. If `NULL`, an unweighted prevalence is calculated.
#'   Default is `NULL`.
#' @param psu Optional character string giving the name of the primary
#'   sampling unit (PSU) variable. If `NULL` (the default), every observation
#'   is treated as its own sampling unit.
#' @param strata Optional character string giving the name of the survey
#'   stratum variable. If `NULL` (the default), no stratification is
#'   specified.
#' @param by Optional character vector giving the names of variables
#'   defining subgroups for which prevalence should be calculated.
#'   If `NULL`, estimates are calculated for the entire analysis
#'   population within each period.
#' @param rolling_year Positive integer specifying the number of calendar years
#'   included in each rolling period. The default is `1`, i.e. prevalence is
#'   calculated separately for each year. Use e.g. `rolling_year = 3` for
#'   three-year rolling periods. Ignored (with a warning if it is not `1`)
#'   when `periods` is supplied.
#' @param periods Optional named list defining your own periods, instead of
#'   rolling periods. Each element is a vector of calendar years and the
#'   element name is the label used for that period in the output, e.g.
#'   `list("2013-2015" = 2013:2015, "2016-2025" = 2016:2025)`. The periods are
#'   kept in the order you give them, which matters for `compare_to = "first"`,
#'   `compare_to = "previous"` and the trend. Periods may have different
#'   lengths and may overlap. If `NULL` (the default), rolling periods
#'   defined by `rolling_year` are used. No recoding of the year variable is
#'   needed.
#' @param ci_level Numeric value between 0 and 1 specifying the confidence
#'   level for the confidence intervals. The default is `0.95`. The
#'   significance level used in the tests is `alpha = 1 - ci_level`.
#' @param digits Number of digits used when rounding the results (percentages
#'   and percentage-point differences). Default is `1`. P-values are not
#'   rounded.
#' @param test Logical. If `TRUE` (the default), significance tests between
#'   periods or subgroups are calculated and returned in `$tests`. If
#'   `FALSE`, `$tests` is `NULL`.
#' @param compare_var Character string giving the dimension along which
#'   prevalence is compared. Either `"rolling_period"` (the default; compare
#'   periods, i.e. change over time) or one of the variables in `by` (compare
#'   subgroups). Comparisons are always made *within* the levels of all other
#'   variables in `by`. For example, with `by = c("gender", "region")` and
#'   `compare_var = "gender"`, men and women are compared within each region
#'   and period. (The period variable is always called `rolling_period` in the
#'   output, also when you define the periods with `periods`.)
#' @param compare_to Defines the reference for the comparisons. One of:
#'   * `"first"` (the default): every level is compared with the first level
#'     (e.g. the earliest period).
#'   * `"previous"`: every level is compared with the level immediately
#'     before it (e.g. the previous period).
#'   * A character string giving a specific level of `compare_var`
#'     (e.g. `"2013-2015"`, a label from `periods`, or `"Female"`): every
#'     other level is compared with that level.
#' @param p_adjust Method for adjusting p-values for multiple comparisons.
#'   Any of [stats::p.adjust.methods], e.g. `"none"` (the default), `"holm"`,
#'   `"bonferroni"` or `"BH"`. The adjustment is applied across all
#'   comparisons in `$tests` (and separately across the trend tests in
#'   `$trend`).
#' @param trend Logical. If `TRUE` (the default), a linear trend test over
#'   the periods is calculated and returned in `$trend`, together with the
#'   fitted trend line and its confidence interval in `$fitted`. The trend is
#'   estimated separately for each combination of the `by` variables. At least
#'   three periods are required, so use `trend = FALSE` when you only define
#'   two periods with `periods`.
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{`estimates`}{A data.table with one row per period (and per
#'     combination of the `by` variables) and the columns
#'     `rolling_period`, the `by` variables, `prevalence` (percent),
#'     `se` (percentage points), `ci_lower`, `ci_upper` (percent) and `n`
#'     (unweighted number of observations).}
#'   \item{`tests`}{A data.table with one row per comparison (or `NULL` if
#'     `test = FALSE` or there is nothing to compare). Columns:
#'     the variables held fixed in the comparison (if any), `reference` and
#'     `compared` (the levels being compared), `prev_ref` and `prev_cmp`
#'     (prevalence in percent), `diff` (`prev_cmp - prev_ref` in percentage
#'     points), `diff_se`, `diff_ci_lower`, `diff_ci_upper`, `z`, `p_value`,
#'     `p_adjusted` and `significant` (`TRUE` if `p_adjusted < 1 - ci_level`).}
#'   \item{`trend`}{A data.table with one row per combination of the `by`
#'     variables (or `NULL` if `trend = FALSE` or fewer than three periods are
#'     available). Columns: the `by` variables, `first_period`, `last_period`,
#'     `n_periods`, `slope` (average change in percentage points per year),
#'     `slope_se`, `slope_ci_lower`, `slope_ci_upper`, `z`, `p_value`,
#'     `p_adjusted`, `significant` and `direction` (`"increasing"`,
#'     `"decreasing"` or `"no significant trend"`).}
#'   \item{`fitted`}{A data.table with the fitted trend line, one row per
#'     period (and per combination of the `by` variables), or `NULL` if
#'     `trend = FALSE` or no trend could be estimated. Columns: the `by`
#'     variables, `rolling_period`, `time` (the mean year of the period, used
#'     as the x value), `prevalence` (the observed estimate, percent),
#'     `fitted` (the value on the trend line, percent) and `lwr`, `upr` (the
#'     confidence interval for the fitted line, percent).}
#' }
#'
#' @details
#' **Analysis population.** The `denominator` argument defines the analysis
#' population for the selected outcome. Only observations with
#' `denominator == 1` are used. The restriction is applied before missing
#' outcomes are removed, periods are constructed and the survey design is
#' created, so the denominator defines the population from which both the
#' prevalence and its variance are estimated. Missing values in `denominator`
#' cause an error. Observations with missing `outcome_var` are removed.
#'
#' **Survey design.** The design is constructed with [survey::svydesign()]
#' (with `nest = TRUE`). Estimates and variances come from [survey::svyby()]
#' and [survey::svymean()]. When PSU and/or strata information are available
#' they should be supplied so that the variance estimator accounts for the
#' complex sampling design.
#'
#' **Rolling periods.** With `rolling_year = 3`, the estimate for 2022 uses
#' observations from 2020 through 2022. Only periods in which all calendar
#' years are present in the data are created. Periods overlap, so one
#' observation can contribute to several periods.
#'
#' **Custom periods.** With `periods` you choose the groups of years
#' yourself, for example to compare a baseline period with all later years.
#' All observations from the listed years are pooled into one estimate for
#' that period. Years listed in `periods` that are not in the (eligible) data
#' give a warning; a period without any observations gives an error. Because
#' the pooled estimate for a long period is a weighted average over its years,
#' it is only comparable with another period if the survey weights are
#' comparable across the years.
#'
#' **Significance tests.** For each comparison the difference in prevalence
#' between two estimates, \eqn{\hat{p}_2 - \hat{p}_1}, is tested with a
#' Wald z-test:
#' \deqn{
#' z = \frac{\hat{p}_2 - \hat{p}_1}{\sqrt{V(\hat{p}_1) + V(\hat{p}_2) -
#' 2\,\mathrm{Cov}(\hat{p}_1, \hat{p}_2)}}.
#' }
#' The covariance is taken from the survey design (via
#' `svyby(..., covmat = TRUE)`). This matters for overlapping periods
#' (rolling periods or overlapping `periods`), because they share
#' observations and their estimates are therefore correlated; ignoring the
#' covariance would give wrong standard errors. Each observation is kept as
#' one sampling unit (or the PSU, if supplied) across all periods it appears
#' in, which is what allows the covariance to be estimated. Confidence
#' intervals for the differences use the same normal approximation and the
#' level given by `ci_level`. The normal approximation is also what
#' [stats::confint()] uses for `svyby` objects, so the tests and the
#' confidence intervals in `$estimates` are consistent.
#'
#' **Trend test.** The trend is the slope of a straight line fitted to the
#' prevalence estimates against the midpoint year of each period (the mean of
#' the years in the period), so `slope` is the average change in percentage
#' points per year. The line is fitted by generalized least squares using the
#' full covariance matrix of the estimates from the survey design, which
#' accounts for the overlap between periods (ordinary regression on
#' overlapping estimates would treat them as independent and give far too
#' small standard errors). The slope is tested with a Wald z-test. This is a
#' test of a *linear* trend; a non-linear development (e.g. flat and then
#' rising) can still give a significant slope, so inspect the estimates as
#' well. The trend is a separate question from the pairwise comparisons
#' controlled by `compare_var`/`compare_to`.
#'
#' **Fitted trend line.** `$fitted` contains the value of the same GLS line
#' at each period, \eqn{\hat{y}_t = \hat{\beta}_0 + \hat{\beta}_1 t}, with
#' a confidence interval \eqn{\hat{y}_t \pm z \cdot SE(\hat{y}_t)}, where
#' \eqn{SE(\hat{y}_t)} comes from the covariance matrix of the estimated
#' coefficients. This is the confidence interval for the *line* (the average
#' development), in the same way as `predict(lm(...), interval = "confidence")`;
#' it is not a prediction interval for a single new estimate and it is not the
#' same as the confidence interval of the observed prevalence in `$estimates`.
#' Because the line is not restricted, `lwr` and `upr` can fall below 0 or
#' above 100 for very low or very high prevalence. The fitted values use
#' exactly the same fit as the slope in `$trend`, so the plotted line and the
#' reported slope always agree. Unlike `lm()`, the fit uses the survey
#' covariance matrix, so the line is not the same as an ordinary least-squares
#' line through the period estimates.
#'
#' **Interpretation.** `significant = TRUE` means that the difference is
#' statistically significant at level `1 - ci_level` after the chosen
#' multiple-comparison adjustment. A statistically significant difference is
#' not necessarily a large or important one; consider the size of `diff` and
#' its confidence interval. When many comparisons are made, consider using
#' `p_adjust`.
#'
#' @examples
#' \dontrun{
#' # Annual prevalence, change over time compared with the first year
#' res <- calc_prevalence(
#'   data = dt,
#'   denominator = "eligible",
#'   year_var = "year",
#'   outcome_var = "outcome",
#'   weight_var = "weight",
#'   rolling_year = 1
#' )
#' res$estimates
#' res$tests
#' res$trend
#' res$fitted   # fitted trend line with confidence interval, e.g. for plotting
#'
#' # Three-year rolling prevalence with PSU and strata, each period
#' # compared with the previous one, Holm-adjusted p-values
#' calc_prevalence(
#'   data = dt,
#'   denominator = "eligible",
#'   year_var = "year",
#'   outcome_var = "outcome",
#'   weight_var = "weight",
#'   psu = "psu",
#'   strata = "stratum",
#'   rolling_year = 3,
#'   compare_to = "previous",
#'   p_adjust = "holm"
#' )
#'
#' # Custom periods: 2013-2015 compared with 2016-2025.
#' # No new year column is needed. Only two periods, so no trend.
#' calc_prevalence(
#'   data = dt,
#'   denominator = "eligible",
#'   year_var = "year",
#'   outcome_var = "outcome",
#'   weight_var = "weight",
#'   periods = list(
#'     "2013-2015" = 2013:2015,
#'     "2016-2025" = 2016:2025
#'   ),
#'   trend = FALSE
#' )
#'
#' # Custom periods by gender: three periods, each compared with the
#' # baseline period, with a trend over the three periods
#' calc_prevalence(
#'   data = dt,
#'   denominator = "eligible",
#'   year_var = "year",
#'   outcome_var = "outcome",
#'   weight_var = "weight",
#'   by = "gender",
#'   periods = list(
#'     "2013-2016" = 2013:2016,
#'     "2017-2020" = 2017:2020,
#'     "2021-2025" = 2021:2025
#'   ),
#'   compare_to = "2013-2016"
#' )
#'
#' # Compare genders within each rolling period
#' calc_prevalence(
#'   data = dt,
#'   denominator = "eligible",
#'   year_var = "year",
#'   outcome_var = "outcome",
#'   weight_var = "weight",
#'   psu = "psu",
#'   strata = "stratum",
#'   by = c("region", "gender"),
#'   rolling_year = 3,
#'   compare_var = "gender",
#'   compare_to = "Male"
#' )
#'
#' # Estimates only, no tests and no trend
#' calc_prevalence(
#'   data = dt, denominator = "eligible", year_var = "year",
#'   outcome_var = "outcome", weight_var = "weight",
#'   test = FALSE, trend = FALSE
#' )$estimates
#' }
#'
#' @importFrom survey svydesign svyby svymean SE
#' @importFrom stats as.formula reformulate coef confint pnorm qnorm p.adjust p.adjust.methods
#' @importFrom data.table as.data.table copy rbindlist setDT set .N
#' @export
calc_prevalence <- function(data,
                            denominator,
                            year_var,
                            outcome_var,
                            weight_var = NULL,
                            psu = NULL,
                            strata = NULL,
                            by = NULL,
                            rolling_year = 1,
                            periods = NULL,
                            ci_level = 0.95,
                            digits = 1,
                            test = TRUE,
                            compare_var = "rolling_period",
                            compare_to = "first",
                            p_adjust = "none",
                            trend = TRUE) {

  # The main function only orchestrates. Each numbered step is done by one
  # small helper (defined below), so every step can be read, tested and
  # debugged on its own.

  # 1. Check the arguments (errors and warnings come from the helper).
  .prev_validate_args(
    data = data, denominator = denominator, year_var = year_var,
    outcome_var = outcome_var, weight_var = weight_var, psu = psu,
    strata = strata, by = by, rolling_year = rolling_year,
    periods = periods, ci_level = ci_level, digits = digits,
    test = test, trend = trend, compare_var = compare_var,
    compare_to = compare_to
  )

  rolling_year <- as.integer(rolling_year)

  # match.arg() gives an informative error for unknown adjustment methods.
  p_adjust <- match.arg(p_adjust, stats::p.adjust.methods)

  # 2. Prepare the data: private copy, eligible population, valid weights.
  variables <- unique(c(denominator, year_var, outcome_var, weight_var,
                        psu, strata, by))

  dt <- .prev_copy_columns(data, variables)
  .prev_check_year(dt, year_var)
  dt <- .prev_restrict_population(dt, denominator, outcome_var)

  weights_out <- .prev_prepare_weights(dt, weight_var)
  dt <- weights_out$data
  weight_var <- weights_out$weight_var

  # 3. Define the periods (rolling or user-defined) and stack them.
  period_info <- .prev_define_periods(dt, year_var, rolling_year, periods)
  roll_data <- .prev_stack_periods(dt, year_var, period_info$defs)

  # 4. Build the survey design and estimate prevalence per domain.
  design <- .prev_build_design(roll_data, weight_var, psu, strata)

  by_vars <- c("rolling_period", by)

  # The covariance between estimates is only needed for tests and trend,
  # so it is skipped when neither is requested.
  need_cov <- test || trend

  result <- .prev_run_svyby(design, outcome_var, by_vars, need_cov)

  estimates_out <- .prev_extract_estimates(
    result = result, roll_data = roll_data, by_vars = by_vars,
    period_labels = period_info$labels, ci_level = ci_level, digits = digits
  )

  # 5. Tests and trend. Both use the covariance matrix of the estimates.
  vc <- if (need_cov) .prev_get_covariance(result, estimates_out$raw) else NULL

  alpha <- 1 - ci_level
  z_crit <- stats::qnorm(1 - alpha / 2)

  tests <- if (test) {
    .prev_run_tests(
      est = estimates_out$raw, vc = vc, by_vars = by_vars,
      compare_var = compare_var, compare_to = compare_to,
      alpha = alpha, z_crit = z_crit, p_adjust = p_adjust, digits = digits
    )
  } else {
    NULL
  }

  trend_out <- if (trend) {
    .prev_run_trend(
      est = estimates_out$raw, vc = vc, group_vars = by,
      period_time = period_info$time,
      alpha = alpha, z_crit = z_crit, p_adjust = p_adjust, digits = digits
    )
  } else {
    list(trend = NULL, fitted = NULL)
  }

  list(
    estimates = estimates_out$table,
    tests = tests,
    trend = trend_out$trend,
    fitted = trend_out$fitted
  )
}


# ======================================================================
# STEP 1: argument validation
# ======================================================================

# Small shared helper: TRUE for exactly one non-missing character string.
# Used many times in the validation, so it is defined once.
.is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

#' Validate the arguments of calc_prevalence()
#'
#' Why: failing early with a clear message is much easier for the user than
#' an obscure error from deep inside the survey package. Keeping all checks
#' in one place also keeps the main function short.
#'
#' How to use: call it with the same arguments as `calc_prevalence()`; it
#' returns `TRUE` invisibly if everything is fine and stops (or warns)
#' otherwise, e.g.
#' `.prev_validate_args(data = df, denominator = "elig", ..., ci_level = 2)`
#' gives an error about `ci_level`.
#'
#' @noRd
.prev_validate_args <- function(data, denominator, year_var, outcome_var,
                                weight_var, psu, strata, by, rolling_year,
                                periods, ci_level, digits, test, trend,
                                compare_var, compare_to) {

  if (!inherits(data, "data.frame")) {
    stop("'data' must be a data.frame or data.table.")
  }

  if (!.is_string(denominator)) {
    stop("'denominator' must be a single character string.")
  }

  if (!.is_string(year_var)) {
    stop("'year_var' must be a single character string.")
  }

  if (!.is_string(outcome_var)) {
    stop("'outcome_var' must be a single character string.")
  }

  if (!is.null(weight_var) && !.is_string(weight_var)) {
    stop("'weight_var' must be NULL or a single character string.")
  }

  if (!is.null(psu) && !.is_string(psu)) {
    stop("'psu' must be NULL or a single character string.")
  }

  if (!is.null(strata) && !.is_string(strata)) {
    stop("'strata' must be NULL or a single character string.")
  }

  if (!is.null(by) && (!is.character(by) || length(by) < 1L)) {
    stop("'by' must be NULL or a character vector of variable names.")
  }

  if (!is.numeric(rolling_year) ||
      length(rolling_year) != 1L ||
      is.na(rolling_year) ||
      rolling_year < 1 ||
      rolling_year != as.integer(rolling_year)) {
    stop("'rolling_year' must be a positive integer.")
  }

  .prev_validate_periods(periods, rolling_year)

  if (!is.numeric(ci_level) ||
      length(ci_level) != 1L ||
      is.na(ci_level) ||
      ci_level <= 0 ||
      ci_level >= 1) {
    stop("'ci_level' must be a single number between 0 and 1.")
  }

  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      digits < 0) {
    stop("'digits' must be a single non-negative number.")
  }

  if (!is.logical(test) || length(test) != 1L || is.na(test)) {
    stop("'test' must be TRUE or FALSE.")
  }

  if (!is.logical(trend) || length(trend) != 1L || is.na(trend)) {
    stop("'trend' must be TRUE or FALSE.")
  }

  # The comparison dimension must exist as an estimation domain: the
  # period variable or one of the 'by' variables.
  if (!.is_string(compare_var) || !compare_var %in% c("rolling_period", by)) {
    stop("'compare_var' must be \"rolling_period\" or one of the variables in 'by'.")
  }

  if (!.is_string(compare_to)) {
    stop("'compare_to' must be a single character string.")
  }

  invisible(TRUE)
}

#' Validate the 'periods' argument
#'
#' Why: `periods` has a more complex structure than the other arguments
#' (a named list of year vectors), so its checks get their own helper.
#' Unique, non-empty names are required because the names become the period
#' labels that identify rows in the output and that `compare_to` refers to.
#'
#' How to use: `.prev_validate_periods(list(a = 2013:2015, b = 2016:2020), 1)`
#' returns `TRUE` invisibly; `NULL` is also valid (rolling periods are used).
#'
#' @noRd
.prev_validate_periods <- function(periods, rolling_year) {

  if (is.null(periods)) {
    return(invisible(TRUE))
  }

  if (!is.list(periods) || length(periods) < 1L) {
    stop("'periods' must be NULL or a non-empty named list of year vectors.")
  }

  period_names <- names(periods)

  if (is.null(period_names) || anyNA(period_names) ||
      any(period_names == "") || anyDuplicated(period_names) > 0L) {
    stop("All elements of 'periods' must have unique, non-empty names.")
  }

  for (nm in period_names) {
    yrs <- periods[[nm]]
    if (!is.numeric(yrs) || length(yrs) < 1L || anyNA(yrs) ||
        any(yrs != round(yrs))) {
      stop(
        "Period '", nm, "' in 'periods' must be a non-empty vector of ",
        "whole-number years without missing values."
      )
    }
  }

  # rolling_year has a default of 1, so only complain when the user
  # clearly tried to use both mechanisms at once.
  if (rolling_year != 1) {
    warning("'rolling_year' is ignored because 'periods' is supplied.",
            call. = FALSE)
  }

  invisible(TRUE)
}


# ======================================================================
# STEP 2: data preparation
# ======================================================================

#' Make a private copy of the needed columns
#'
#' Why: later steps add columns by reference (`data.table::set()`).
#' `as.data.table()` returns a data.table input unchanged, so without
#' `copy()` the user's own data would be modified. Keeping only the needed
#' columns also saves memory when periods are stacked later.
#'
#' How to use: `.prev_copy_columns(df, c("year", "outcome"))` returns a
#' data.table with just those columns; it stops if a column is missing.
#'
#' @noRd
.prev_copy_columns <- function(data, variables) {

  missing_variables <- setdiff(variables, names(data))

  if (length(missing_variables) > 0L) {
    stop(
      "The following variables are not present in 'data': ",
      paste(missing_variables, collapse = ", ")
    )
  }

  dt <- data.table::copy(data.table::as.data.table(data))
  dt[, variables, with = FALSE]
}

#' Check the year variable
#'
#' Why: periods are built from whole calendar years, so the year must be
#' numeric, complete and whole-numbered (2020.5 would silently break the
#' period logic).
#'
#' How to use: `.prev_check_year(dt, "year")`; returns `TRUE` invisibly or
#' stops with a message.
#'
#' @noRd
.prev_check_year <- function(dt, year_var) {

  if (!is.numeric(dt[[year_var]])) {
    stop("'year_var' must contain numeric/integer calendar years.")
  }

  if (anyNA(dt[[year_var]])) {
    stop("'year_var' contains missing values.")
  }

  if (any(dt[[year_var]] != round(dt[[year_var]]))) {
    stop("'year_var' must contain whole-number calendar years.")
  }

  invisible(TRUE)
}

#' Restrict the data to the analysis population
#'
#' Why: the denominator defines who the prevalence applies to. It must be
#' applied BEFORE periods and the survey design are built, so that it
#' determines both the estimate and its variance. Observations with a
#' missing outcome cannot contribute to a prevalence and are dropped here
#' (after the denominator restriction).
#'
#' How to use: `.prev_restrict_population(dt, "eligible", "outcome")`
#' returns the rows with `eligible == 1` and a non-missing 0/1 outcome.
#' It stops if the denominator has NA or is not 0/1, if the outcome is not
#' 0/1, or if no rows remain.
#'
#' @noRd
.prev_restrict_population <- function(dt, denominator, outcome_var) {

  # NA in the denominator would leave it unclear who belongs to the
  # population, so it is an error rather than a silent exclusion.
  denominator_values <- unique(dt[[denominator]])

  if (anyNA(denominator_values)) {
    stop(
      "'denominator' contains missing values. ",
      "Please code the denominator as 0 or 1 before running the function."
    )
  }

  if (!all(denominator_values %in% c(0, 1))) {
    stop(
      "'denominator' must be coded as 0/1. Found values: ",
      paste(sort(denominator_values), collapse = ", ")
    )
  }

  # The logical vector is made outside dt[...] so that a column with the
  # same name cannot be picked up by accident inside the data.table call.
  .keep <- dt[[denominator]] == 1
  dt <- dt[.keep]

  if (nrow(dt) == 0L) {
    stop("No observations remain after restricting to ", denominator, " == 1.")
  }

  outcome_values <- unique(dt[[outcome_var]][!is.na(dt[[outcome_var]])])

  if (length(outcome_values) == 0L) {
    stop(
      "No non-missing observations remain for 'outcome_var' ",
      "within the denominator population."
    )
  }

  # A 0/1 outcome is what makes the weighted mean a prevalence.
  if (!all(outcome_values %in% c(0, 1))) {
    stop(
      "'outcome_var' must be coded as 0/1. Found values: ",
      paste(sort(outcome_values), collapse = ", ")
    )
  }

  .keep <- !is.na(dt[[outcome_var]])
  dt <- dt[.keep]

  if (nrow(dt) == 0L) {
    stop(
      "No observations remain after removing missing values ",
      "from 'outcome_var'."
    )
  }

  dt
}

#' Prepare and validate the survey weights
#'
#' Why: weights must be positive and finite. For an unweighted analysis
#' every observation gets weight 1 in a private column (".weight"), so a
#' user variable can never be overwritten. The check runs only on the rows
#' that are actually used, so a missing weight on a dropped row is no
#' problem.
#'
#' How to use: `.prev_prepare_weights(dt, NULL)` or
#' `.prev_prepare_weights(dt, "weight")`. Returns a list with `data` (the
#' data.table) and `weight_var` (the weight column name to use).
#'
#' @noRd
.prev_prepare_weights <- function(dt, weight_var) {

  if (is.null(weight_var)) {
    data.table::set(dt, j = ".weight", value = 1)
    weight_var <- ".weight"
  }

  weights <- dt[[weight_var]]

  if (anyNA(weights) || any(!is.finite(weights))) {
    stop("'weight_var' contains missing, infinite, or non-finite values.")
  }

  if (any(weights <= 0)) {
    stop("'weight_var' must contain positive values.")
  }

  list(data = dt, weight_var = weight_var)
}


# ======================================================================
# STEP 3: periods
# ======================================================================

#' Create rolling periods
#'
#' Why: rolling periods smooth year-to-year noise. A period ending in year E
#' (length k) is only created if every year E-k+1, ..., E is present, so a
#' "3-year" period never silently covers fewer than three years.
#'
#' How to use: `.prev_rolling_periods(2013:2017, 3)` returns a named list:
#' `"2013-2015" = 2013:2015`, `"2014-2016" = 2014:2016`,
#' `"2015-2017" = 2015:2017`. With `rolling_year = 1` every year is its own
#' period (`"2013-2013"`, ...).
#'
#' @noRd
.prev_rolling_periods <- function(years, rolling_year) {

  if (length(years) < rolling_year) {
    stop(
      "The denominator population contains ", length(years),
      " unique year(s), but 'rolling_year' is ", rolling_year, "."
    )
  }

  end_years <- years[
    vapply(
      years,
      function(end_year) {
        all(seq.int(end_year - rolling_year + 1L, end_year) %in% years)
      },
      logical(1)
    )
  ]

  if (length(end_years) == 0L) {
    stop(
      "No complete ", rolling_year,
      "-year rolling periods can be constructed from the available years."
    )
  }

  defs <- lapply(
    end_years,
    function(end_year) seq.int(end_year - rolling_year + 1L, end_year)
  )

  names(defs) <- vapply(
    defs,
    function(yrs) sprintf("%d-%d", as.integer(min(yrs)), as.integer(max(yrs))),
    character(1)
  )

  defs
}

#' Check and clean user-defined periods
#'
#' Why: the user may list years that are not in the data. A period with no
#' data at all is an error (nothing to estimate). A partly missing period is
#' allowed but flagged with a warning, because the user may otherwise think
#' the estimate covers all listed years. The years are reduced to those
#' actually present.
#'
#' How to use: `.prev_custom_periods(list(a = 2013:2015, b = 2016:2025),
#' years = 2013:2024)` returns the list with `b` reduced to 2016:2024 and a
#' warning about 2025.
#'
#' @noRd
.prev_custom_periods <- function(periods, years) {

  defs <- lapply(names(periods), function(nm) {

    wanted <- sort(unique(periods[[nm]]))
    present <- intersect(wanted, years)

    if (length(present) == 0L) {
      stop(
        "Period '", nm, "' contains no observations in the eligible ",
        "data (years requested: ", paste(range(wanted), collapse = "-"), ")."
      )
    }

    if (length(present) < length(wanted)) {
      warning(
        "Period '", nm, "': no data for year(s) ",
        paste(setdiff(wanted, present), collapse = ", "),
        ". The estimate uses only the years that are present.",
        call. = FALSE
      )
    }

    present
  })

  names(defs) <- names(periods)
  defs
}

#' Define the periods used in the analysis
#'
#' Why: rolling and user-defined periods are turned into the same structure
#' (named list: label -> years), so the rest of the function treats them
#' identically. It also returns the labels in the intended order (used for
#' "first"/"previous" comparisons and the trend, so it must not become
#' alphabetical) and one numeric time point per period (the mean year, used
#' as the x value in the trend).
#'
#' How to use: `.prev_define_periods(dt, "year", rolling_year = 3, periods =
#' NULL)`. Returns a list with `defs`, `labels` and `time`.
#'
#' @noRd
.prev_define_periods <- function(dt, year_var, rolling_year, periods) {

  years <- sort(unique(dt[[year_var]]))

  defs <- if (is.null(periods)) {
    .prev_rolling_periods(years, rolling_year)
  } else {
    .prev_custom_periods(periods, years)
  }

  list(
    defs = defs,
    labels = names(defs),
    # For rolling periods the mean year differs from the end year by a
    # constant, which does not change the trend slope.
    time = vapply(defs, mean, numeric(1))
  )
}

#' Stack all periods into one data set
#'
#' Why: svyby() estimates all periods in one call when they are stacked with
#' a `rolling_period` column. Periods can overlap, so one observation can
#' appear in several periods. Every original observation therefore gets a
#' unique `.obs_id` BEFORE stacking; the copies share it, which lets the
#' survey design estimate the covariance between overlapping periods (needed
#' for valid tests and trend).
#'
#' How to use: `.prev_stack_periods(dt, "year", period_info$defs)` returns a
#' plain data.frame with the extra columns `.obs_id` and `rolling_period`.
#'
#' @noRd
.prev_stack_periods <- function(dt, year_var, period_defs) {

  data.table::set(dt, j = ".obs_id", value = seq_len(nrow(dt)))

  stacked <- data.table::rbindlist(
    lapply(names(period_defs), function(label) {

      in_period <- dt[[year_var]] %in% period_defs[[label]]
      tmp <- dt[in_period]

      data.table::set(tmp, j = "rolling_period", value = label)

      tmp
    }),
    use.names = TRUE,
    fill = FALSE
  )

  # The survey package works on plain data frames.
  as.data.frame(stacked)
}


# ======================================================================
# STEP 4: survey design and estimation
# ======================================================================

#' Build the survey design
#'
#' Why: the design tells survey how the data were sampled so that variances
#' are correct. Without a PSU every original observation is its own sampling
#' unit (same variance per period as `ids = ~1`), but copies of the same
#' observation share a cluster across periods, which makes the covariance
#' between overlapping periods estimable. `nest = TRUE` allows PSU ids that
#' repeat across strata.
#'
#' How to use: `.prev_build_design(roll_data, "weight", psu = "psu", strata =
#' "stratum")` returns a `survey.design` object.
#'
#' @noRd
.prev_build_design <- function(roll_data, weight_var, psu, strata) {

  ids_formula <- if (is.null(psu)) {
    stats::as.formula("~.obs_id")
  } else {
    stats::as.formula(paste0("~", psu))
  }

  strata_formula <- if (is.null(strata)) {
    NULL
  } else {
    stats::as.formula(paste0("~", strata))
  }

  survey::svydesign(
    ids = ids_formula,
    strata = strata_formula,
    weights = stats::as.formula(paste0("~", weight_var)),
    data = roll_data,
    nest = TRUE
  )
}

#' Estimate prevalence per period and subgroup
#'
#' Why: wraps `survey::svyby()` so the covariance matrix (needed for tests
#' and trend) is requested only when needed, and so a failure to compute it
#' degrades to a warning instead of stopping the whole analysis.
#'
#' How to use: `.prev_run_svyby(design, "outcome", c("rolling_period",
#' "gender"), need_cov = TRUE)` returns the `svyby` result.
#'
#' @noRd
.prev_run_svyby <- function(design, outcome_var, by_vars, need_cov) {

  by_formula <- stats::reformulate(by_vars)

  run <- function(covmat) {
    survey::svyby(
      formula = stats::as.formula(paste0("~", outcome_var)),
      by = by_formula,
      design = design,
      FUN = survey::svymean,
      vartype = "se",
      na.rm = TRUE,
      covmat = covmat
    )
  }

  tryCatch(
    run(covmat = need_cov),
    error = function(e) {
      if (!need_cov) stop(e)
      warning(
        "Could not compute covariances between estimates (",
        conditionMessage(e), "). Tests assume independent estimates, which ",
        "is not valid for overlapping periods.",
        call. = FALSE
      )
      run(covmat = FALSE)
    }
  )
}

#' Extract estimates and confidence intervals from a svyby result
#'
#' Why: svyby returns a special object. This converts it into (a) a raw
#' data.frame on the proportion scale, used internally so no rounding error
#' enters the tests, and (b) the rounded percent table returned to the user.
#' The period is made a factor in the user's order, because svyby would
#' otherwise sort labels alphabetically (wrong for e.g. "Baseline" and
#' "Follow-up"). The unweighted n is matched on a text key rather than
#' merged, so the row order stays identical to the covariance matrix.
#'
#' How to use: `.prev_extract_estimates(result, roll_data, by_vars, labels,
#' 0.95, 1)` returns a list with `raw` (columns `.est`, `.se`) and `table`
#' (a data.table with `prevalence`, `se`, `ci_lower`, `ci_upper`, `n`).
#'
#' @noRd
.prev_extract_estimates <- function(result, roll_data, by_vars,
                                    period_labels, ci_level, digits) {

  est_raw <- as.data.frame(result)[, by_vars, drop = FALSE]

  est_raw$rolling_period <- factor(
    as.character(est_raw$rolling_period),
    levels = period_labels
  )

  est_raw$.est <- as.numeric(stats::coef(result))
  est_raw$.se <- as.numeric(survey::SE(result))

  # confint() on a svyby object uses the normal approximation, matching the
  # Wald tests later on.
  ci <- stats::confint(result, level = ci_level)

  make_key <- function(x) {
    do.call(paste, c(lapply(x[by_vars], as.character), sep = "\r"))
  }

  n_tab <- as.data.frame(
    data.table::as.data.table(roll_data)[, .N, by = by_vars]
  )

  table <- est_raw[, by_vars, drop = FALSE]
  table$rolling_period <- as.character(table$rolling_period)
  table$prevalence <- round(est_raw$.est * 100, digits)
  table$se <- round(est_raw$.se * 100, digits)
  table$ci_lower <- round(ci[, 1L] * 100, digits)
  table$ci_upper <- round(ci[, 2L] * 100, digits)
  table$n <- n_tab$N[match(make_key(est_raw), make_key(n_tab))]

  list(raw = est_raw, table = data.table::as.data.table(table))
}

#' Get the covariance matrix of the estimates
#'
#' Why: tests of differences and the trend need Var and Cov of the
#' estimates. If svyby did not return the matrix, a diagonal matrix
#' (independent estimates) is used with a warning, because that is only
#' valid for non-overlapping periods.
#'
#' How to use: `.prev_get_covariance(result, est_raw)` returns a square
#' matrix with one row/column per row of `est_raw` (same order).
#'
#' @noRd
.prev_get_covariance <- function(result, est_raw) {

  vc <- attr(result, "var")

  if (is.null(vc) || !is.matrix(vc) || nrow(vc) != nrow(est_raw)) {
    warning(
      "No covariance matrix available; tests assume independent ",
      "estimates, which is not valid for overlapping periods.",
      call. = FALSE
    )
    vc <- diag(est_raw$.se^2, nrow = nrow(est_raw))
  }

  vc
}


# ======================================================================
# STEP 5a: building blocks for the tests (pure functions, easy to test)
# ======================================================================

#' Group row numbers by the combinations of some variables
#'
#' Why: both the pairwise tests and the trend are done separately within
#' subgroups (e.g. per gender). This returns the row numbers of each
#' subgroup, or a single group with all rows when there are no variables.
#'
#' How to use: `.prev_split_rows(est, c("gender"))` returns a list of integer
#' vectors, one per gender; `.prev_split_rows(est, character(0))` returns
#' `list(seq_len(nrow(est)))`.
#'
#' @noRd
.prev_split_rows <- function(est, vars) {

  all_rows <- seq_len(nrow(est))

  if (length(vars) == 0L) {
    return(list(all_rows))
  }

  unname(split(all_rows, lapply(vars, function(v) est[[v]]), drop = TRUE))
}

#' Wald test of a difference
#'
#' Why: one place for the z-test used by every comparison. `pmax()` guards
#' against tiny negative variances from rounding error, and a zero standard
#' error gives `NA` instead of `Inf`.
#'
#' How to use: `.prev_wald_test(diff = 0.02, variance = 0.0001)` returns a
#' list with `se` (0.01), `z` (2) and a two-sided `p_value` (about 0.0455).
#'
#' @noRd
.prev_wald_test <- function(diff, variance) {

  se <- sqrt(pmax(variance, 0))
  z <- ifelse(se > 0, diff / se, NA_real_)

  list(se = se, z = z, p_value = 2 * stats::pnorm(-abs(z)))
}

#' Fit a straight line by generalized least squares
#'
#' Why: the trend needs a line fitted to estimates that are correlated
#' (overlapping periods). GLS uses the full covariance matrix `V`:
#' beta = (X' V^-1 X)^-1 X' V^-1 y. The time variable is centred for
#' numerical stability (this does not change the slope). Returns `NULL` if
#' a matrix is singular, so the caller can skip that subgroup.
#'
#' How to use: `.prev_gls_line(y = c(.01, .02, .04), V = diag(1e-5, 3),
#' time = c(2014, 2016, 2018))` returns `slope`, `slope_se` and the fitted
#' values `fit` with their standard errors `fit_se` (proportion scale).
#' With `V` diagonal it equals weighted least squares.
#'
#' @noRd
.prev_gls_line <- function(y, V, time) {

  X <- cbind(1, time - mean(time))

  V_inv <- tryCatch(solve(V), error = function(e) NULL)
  if (is.null(V_inv)) {
    return(NULL)
  }

  cov_beta <- tryCatch(solve(t(X) %*% V_inv %*% X), error = function(e) NULL)
  if (is.null(cov_beta)) {
    return(NULL)
  }

  beta <- cov_beta %*% t(X) %*% V_inv %*% y

  list(
    slope = as.numeric(beta[2L]),
    slope_se = sqrt(max(cov_beta[2L, 2L], 0)),
    # Fitted values X beta; their variance is the diagonal of X Var(beta) X'.
    # rowSums((X C) * X) computes that diagonal without the full matrix.
    fit = as.numeric(X %*% beta),
    fit_se = sqrt(pmax(rowSums((X %*% cov_beta) * X), 0))
  )
}

#' Decide which pairs of rows are compared
#'
#' Why: separates the "who is compared with whom" logic from the statistics.
#' Rows must already be in the intended order (see `.prev_compare_estimates`).
#'
#' How to use: `.prev_make_pairs(rows = 1:4, lev = c("a","b","c","d"),
#' compare_to = "first")` returns a data.frame with columns `ref` and `cmp`
#' (row numbers). `"previous"` pairs each level with the one before it; a
#' level name pairs every other level with that level. Returns `NULL` if the
#' named level is not present.
#'
#' @noRd
.prev_make_pairs <- function(rows, lev, compare_to) {

  if (identical(compare_to, "first")) {
    return(data.frame(ref = rows[1L], cmp = rows[-1L]))
  }

  if (identical(compare_to, "previous")) {
    return(data.frame(ref = rows[-length(rows)], cmp = rows[-1L]))
  }

  ref_row <- rows[lev == compare_to]

  if (length(ref_row) != 1L) {
    return(NULL)
  }

  data.frame(ref = ref_row, cmp = setdiff(rows, ref_row))
}


# ======================================================================
# STEP 5b: pairwise comparisons
# ======================================================================

#' Pairwise Wald tests between estimates
#'
#' Why: answers "is the prevalence different between two periods (or two
#' subgroups)?". Var(a - b) = Var(a) + Var(b) - 2 Cov(a, b); the covariance
#' term makes the test valid for overlapping periods. Comparisons are made
#' within the levels of all other variables (`strata_vars`).
#'
#' How to use: `.prev_compare_estimates(est_raw, vc, "rolling_period",
#' strata_vars = "gender", compare_to = "first")`. `est_raw` needs `.est`,
#' `.se` and the domain variables (period as a factor in the intended
#' order). Returns a data.frame on the proportion scale (unrounded) or NULL.
#'
#' @noRd
.prev_compare_estimates <- function(est, vc, compare_var, strata_vars,
                                    compare_to) {

  results <- lapply(.prev_split_rows(est, strata_vars), function(rows) {

    # Order the levels so that "first" and "previous" are well defined:
    # factors keep their level order, other types are sorted.
    x <- est[[compare_var]][rows]
    rows <- rows[if (is.factor(x)) order(as.integer(x)) else order(x)]
    lev <- as.character(est[[compare_var]][rows])

    if (length(rows) < 2L) {
      return(NULL)
    }

    pairs <- .prev_make_pairs(rows, lev, compare_to)
    if (is.null(pairs)) {
      return(NULL)
    }

    diff <- est$.est[pairs$cmp] - est$.est[pairs$ref]

    variance <- vc[cbind(pairs$cmp, pairs$cmp)] +
      vc[cbind(pairs$ref, pairs$ref)] -
      2 * vc[cbind(pairs$cmp, pairs$ref)]

    wald <- .prev_wald_test(diff, variance)

    # Keep the fixed variables so each test can be matched to its subgroup.
    res <- if (length(strata_vars) > 0L) {
      est[pairs$cmp, strata_vars, drop = FALSE]
    } else {
      data.frame(row.names = seq_len(nrow(pairs)))
    }

    res$reference <- as.character(est[[compare_var]][pairs$ref])
    res$compared <- as.character(est[[compare_var]][pairs$cmp])
    res$prev_ref <- est$.est[pairs$ref]
    res$prev_cmp <- est$.est[pairs$cmp]
    res$diff <- diff
    res$diff_se <- wald$se
    res$z <- wald$z
    res$p_value <- wald$p_value

    rownames(res) <- NULL
    res
  })

  results <- Filter(Negate(is.null), results)

  if (length(results) == 0L) {
    return(NULL)
  }

  as.data.frame(data.table::rbindlist(results, use.names = TRUE))
}

#' Adjust, flag and format the pairwise test results
#'
#' Why: significance is decided on the UNROUNDED values; rounding and the
#' conversion to percent / percentage points happen last, only for
#' presentation. Identifying columns are placed first.
#'
#' How to use: `.prev_format_tests(tests_raw, alpha = 0.05, z_crit = 1.96,
#' p_adjust = "holm", digits = 1)` takes the output of
#' `.prev_compare_estimates()` and returns the final data.table.
#'
#' @noRd
.prev_format_tests <- function(tests, alpha, z_crit, p_adjust, digits) {

  tests$p_adjusted <- stats::p.adjust(tests$p_value, method = p_adjust)
  tests$significant <- tests$p_adjusted < alpha

  tests$diff_ci_lower <- round((tests$diff - z_crit * tests$diff_se) * 100, digits)
  tests$diff_ci_upper <- round((tests$diff + z_crit * tests$diff_se) * 100, digits)
  tests$prev_ref <- round(tests$prev_ref * 100, digits)
  tests$prev_cmp <- round(tests$prev_cmp * 100, digits)
  tests$diff <- round(tests$diff * 100, digits)
  tests$diff_se <- round(tests$diff_se * 100, digits)
  tests$z <- round(tests$z, 2)

  stat_cols <- c("prev_ref", "prev_cmp", "diff", "diff_se",
                 "diff_ci_lower", "diff_ci_upper", "z", "p_value",
                 "p_adjusted", "significant")

  data.table::as.data.table(
    tests[, c(setdiff(names(tests), stat_cols), stat_cols)]
  )
}

#' Run the pairwise tests (compare + format + warn if nothing to compare)
#'
#' Why: groups the three steps the main function needs for `$tests`, and
#' turns "nothing to compare" into a clear warning with `NULL` output.
#'
#' How to use: called by `calc_prevalence()`; returns the final `$tests`
#' data.table or `NULL`.
#'
#' @noRd
.prev_run_tests <- function(est, vc, by_vars, compare_var, compare_to,
                            alpha, z_crit, p_adjust, digits) {

  # Everything except the comparison dimension is held fixed, so e.g.
  # genders are compared within each period.
  tests <- .prev_compare_estimates(
    est = est, vc = vc, compare_var = compare_var,
    strata_vars = setdiff(by_vars, compare_var), compare_to = compare_to
  )

  if (is.null(tests) || nrow(tests) == 0L) {
    warning(
      "No comparisons could be made: '", compare_var, "' has fewer than ",
      "two levels (or the reference level in 'compare_to' was not found).",
      call. = FALSE
    )
    return(NULL)
  }

  .prev_format_tests(tests, alpha, z_crit, p_adjust, digits)
}


# ======================================================================
# STEP 5c: linear trend and fitted line
# ======================================================================

#' Linear trend and fitted line per subgroup
#'
#' Why: answers "is there a trend over the whole series?". One GLS line is
#' fitted per subgroup (`.prev_gls_line`), and both the slope table and the
#' fitted values come from that same fit, so a plotted line always agrees
#' with the reported slope. At least three periods are required.
#'
#' How to use: `.prev_trend_estimates(est_raw, vc, group_vars = "gender",
#' period_time)` where `period_time` is a named numeric vector (period label
#' -> mean year). Returns `NULL`, or a list with data.frames `trend` and
#' `fitted` (proportion scale, unrounded).
#'
#' @noRd
.prev_trend_estimates <- function(est, vc, group_vars, period_time) {

  results <- lapply(.prev_split_rows(est, group_vars), function(rows) {

    # Sort chronologically so first_period / last_period are correct.
    time <- unname(period_time[as.character(est$rolling_period[rows])])
    ord <- order(time)
    rows <- rows[ord]
    time <- time[ord]

    # A line through two points is not a test; require three.
    if (length(rows) < 3L) {
      return(NULL)
    }

    y <- est$.est[rows]
    gls <- .prev_gls_line(y = y, V = vc[rows, rows, drop = FALSE], time = time)

    if (is.null(gls)) {
      return(NULL)
    }

    wald <- .prev_wald_test(gls$slope, gls$slope_se^2)

    # Subgroup identifiers, so each result can be matched to its subgroup.
    id_df <- function(n_rows, source_rows) {
      if (length(group_vars) > 0L) {
        est[source_rows, group_vars, drop = FALSE]
      } else {
        data.frame(row.names = seq_len(n_rows))
      }
    }

    res <- id_df(1L, rows[1L])
    res$first_period <- as.character(est$rolling_period[rows[1L]])
    res$last_period <- as.character(est$rolling_period[rows[length(rows)]])
    res$n_periods <- length(rows)
    res$slope <- gls$slope
    res$slope_se <- gls$slope_se
    res$z <- wald$z
    res$p_value <- wald$p_value
    rownames(res) <- NULL

    fit_df <- id_df(length(rows), rows)
    fit_df$rolling_period <- as.character(est$rolling_period[rows])
    fit_df$time <- time
    fit_df$observed <- y
    fit_df$fit <- gls$fit
    fit_df$fit_se <- gls$fit_se
    rownames(fit_df) <- NULL

    list(trend = res, fitted = fit_df)
  })

  trend_rows <- Filter(Negate(is.null), lapply(results, function(r) r$trend))
  fitted_rows <- Filter(Negate(is.null), lapply(results, function(r) r$fitted))

  if (length(trend_rows) == 0L) {
    return(NULL)
  }

  list(
    trend = as.data.frame(data.table::rbindlist(trend_rows, use.names = TRUE)),
    fitted = as.data.frame(data.table::rbindlist(fitted_rows, use.names = TRUE))
  )
}

#' Adjust, flag and format the trend results
#'
#' Why: same principle as `.prev_format_tests()`: decide on unrounded values,
#' then convert to percent and round. Adds a plain-language `direction`
#' (only "increasing"/"decreasing" when the slope is significant) and the
#' confidence interval of the fitted line, `fit +/- z * SE(fit)`.
#'
#' How to use: `.prev_format_trend(trend_raw, alpha, z_crit, p_adjust,
#' digits)` takes the output of `.prev_trend_estimates()` and returns
#' `list(trend = <data.table>, fitted = <data.table>)`.
#'
#' @noRd
.prev_format_trend <- function(trend_raw, alpha, z_crit, p_adjust, digits) {

  trend_tab <- trend_raw$trend
  fitted_tab <- trend_raw$fitted

  # ---- slope table ------------------------------------------------------
  trend_tab$p_adjusted <- stats::p.adjust(trend_tab$p_value, method = p_adjust)
  trend_tab$significant <- trend_tab$p_adjusted < alpha

  trend_tab$direction <- ifelse(
    !is.na(trend_tab$significant) & trend_tab$significant,
    ifelse(trend_tab$slope > 0, "increasing", "decreasing"),
    "no significant trend"
  )

  trend_tab$slope_ci_lower <- round((trend_tab$slope - z_crit * trend_tab$slope_se) * 100, digits)
  trend_tab$slope_ci_upper <- round((trend_tab$slope + z_crit * trend_tab$slope_se) * 100, digits)
  trend_tab$slope <- round(trend_tab$slope * 100, digits)
  trend_tab$slope_se <- round(trend_tab$slope_se * 100, digits)
  trend_tab$z <- round(trend_tab$z, 2)

  trend_stats <- c("slope", "slope_se", "slope_ci_lower", "slope_ci_upper",
                   "z", "p_value", "p_adjusted", "significant", "direction")

  # ---- fitted line ------------------------------------------------------
  fitted_tab$lwr <- round((fitted_tab$fit - z_crit * fitted_tab$fit_se) * 100, digits)
  fitted_tab$upr <- round((fitted_tab$fit + z_crit * fitted_tab$fit_se) * 100, digits)
  fitted_tab$fitted <- round(fitted_tab$fit * 100, digits)
  fitted_tab$prevalence <- round(fitted_tab$observed * 100, digits)

  fitted_stats <- c("time", "observed", "fit", "fit_se", "lwr", "upr",
                    "fitted", "prevalence")
  fitted_ids <- setdiff(names(fitted_tab), fitted_stats)

  list(
    trend = data.table::as.data.table(
      trend_tab[, c(setdiff(names(trend_tab), trend_stats), trend_stats)]
    ),
    fitted = data.table::as.data.table(
      fitted_tab[, c(fitted_ids, "time", "prevalence", "fitted", "lwr", "upr")]
    )
  )
}

#' Run the trend (estimate + format + warn if no trend)
#'
#' Why: groups the steps the main function needs for `$trend` and `$fitted`
#' and turns "no trend possible" into a clear warning with `NULL` output.
#'
#' How to use: called by `calc_prevalence()`; returns
#' `list(trend = ..., fitted = ...)` (both possibly `NULL`).
#'
#' @noRd
.prev_run_trend <- function(est, vc, group_vars, period_time,
                            alpha, z_crit, p_adjust, digits) {

  trend_raw <- .prev_trend_estimates(est, vc, group_vars, period_time)

  if (is.null(trend_raw) || nrow(trend_raw$trend) == 0L) {
    warning(
      "No trend could be estimated: at least three periods and a ",
      "non-singular covariance matrix are required.",
      call. = FALSE
    )
    return(list(trend = NULL, fitted = NULL))
  }

  .prev_format_trend(trend_raw, alpha, z_crit, p_adjust, digits)
}


# ======================================================================
# Example of testing a helper (testthat), e.g. tests/testthat/test-helpers.R
# ======================================================================
#
# test_that("Wald test gives z = 2 and p about 0.0455", {
#   w <- .prev_wald_test(diff = 0.02, variance = 0.0001)
#   expect_equal(w$z, 2)
#   expect_equal(w$p_value, 0.0455, tolerance = 1e-3)
# })
#
# test_that("rolling periods need complete years", {
#   p <- .prev_rolling_periods(c(2013, 2014, 2016), rolling_year = 2)
#   expect_equal(names(p), "2013-2014")      # 2015 is missing
# })
#
# test_that("GLS with diagonal V recovers an exact line", {
#   g <- .prev_gls_line(y = c(.01, .02, .03), V = diag(1e-6, 3), time = 1:3)
#   expect_equal(g$slope, 0.01)
# })
