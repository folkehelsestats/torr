#' Calculate prevalence and confidence intervals
#'
#' Calculates survey-weighted prevalence estimates and confidence intervals
#' for a binary outcome over single-year or rolling multi-year periods.
#' Different analysis populations can be specified for different outcomes
#' through the `denominator` variable.
#'
#' @param data A data frame or data.table containing the survey data.
#' @param denominator Character string giving the name of a binary
#'   denominator/eligibility variable. Observations with `denominator == 1`
#'   are included in the analysis population; observations with
#'   `denominator == 0` are excluded. The denominator variable therefore
#'   defines the population to which the prevalence estimate applies.
#' @param year_var Character string giving the name of the variable
#'   containing the calendar year.
#' @param outcome_var Character string giving the name of the binary
#'   outcome variable. The outcome must be coded as `0` and `1`, where
#'   `1` indicates the outcome of interest.
#' @param weight_var Character string giving the name of the survey
#'   weight variable. If NULL then run an uweighted prevalence. Default is NULL.
#' @param psu Optional character string giving the name of the primary
#'   sampling unit (PSU) variable. If `NULL` (the default), `ids = ~1`
#'   is used and no clustering is specified.
#' @param strata Optional character string giving the name of the survey
#'   stratum variable. If `NULL` (the default), no stratification is
#'   specified.
#' @param by Optional character vector giving the names of variables
#'   defining subgroups for which prevalence should be calculated.
#'   If `NULL`, estimates are calculated for the entire analysis
#'   population within each rolling period.
#' @param rolling_year Positive integer specifying the number of calendar years
#'   included in each rolling period. The default is `3`. Use `rolling_year = 1`
#'   to calculate prevalence separately for each year.
#' @param ci_level Numeric value between 0 and 1 specifying the confidence
#'   level for the confidence intervals. The default is `0.95`.
#'
#' @return A data frame containing the prevalence estimates and confidence
#'   intervals produced by [survey::svyby()]. The prevalence estimate is
#'   the survey-weighted mean of `outcome_var`. For a 0/1 outcome this is
#'   equivalent to prevalence. Confidence interval columns are typically
#'   named `ci.2.5` and `ci.97.5` for a 95\% confidence interval.
#'
#' @details
#' The `denominator` argument defines the analysis population for the
#' selected outcome. Only observations with `denominator == 1` are used
#' in the prevalence calculation. This allows different outcomes to have
#' different eligible populations. For example, one outcome may be
#' applicable to the entire survey population while another may only be
#' applicable to a particular age group or other eligible population.
#'
#' The denominator variable is applied before missing outcomes are removed,
#' rolling periods are constructed, and the survey design is created.
#' Therefore, the denominator defines the population from which the
#' prevalence and its variance are estimated.
#'
#' The survey design is constructed using [survey::svydesign()]. If `psu`
#' is not supplied, `ids = ~1` is used, treating observations as independent
#' sampling units. If `psu` is supplied, it is used as the primary sampling
#' unit. If `strata` is supplied, it is used to account for stratification.
#'
#' The confidence intervals are based on the survey-design variance
#' estimator used by [survey::svymean()] and [survey::svyby()]. They are
#' therefore different from confidence intervals calculated using an
#' effective sample size, such as
#' \deqn{
#' SE = \sqrt{p(1-p)/n_{\mathrm{eff}}},
#' }
#' where
#' \deqn{
#' n_{\mathrm{eff}} =
#' \frac{(\sum w_i)^2}{\sum w_i^2}.
#' }
#'
#' When PSU and/or strata information are available from the original
#' survey design, they should be supplied because they allow the variance
#' estimator to account for the complex sample design.
#'
#' Rolling rolling_years are defined using calendar years. For example, with
#' `rolling_year = 3`, the estimate for 2022 uses observations from 2020 through
#' 2022. With `rolling_year = 1`, each year is treated as its own estimation
#' period.
#'
#' Note that rolling periods overlap. For example, an observation from
#' 2020 can contribute to the 2018--2020, 2019--2021, and 2020--2022
#' estimates. The estimates from overlapping periods should therefore not
#' be treated as statistically independent.
#'
#' Missing values in `denominator` are not treated as eligible and cause
#' an error because the analysis population cannot be determined.
#' Observations with missing values in `outcome_var` are removed because
#' they cannot contribute to the prevalence estimate.
#'
#' The outcome must be coded as 0/1. For such an outcome, the
#' survey-weighted mean is the prevalence of the outcome.
#'
#' @examples
#' # Annual prevalence for an outcome with a specific denominator
#' calc_prevalence(
#'   data = dt,
#'   denominator = "eligible",
#'   year_var = "year",
#'   outcome_var = "outcome",
#'   weight_var = "weight",
#'   rolling_year = 1
#' )
#'
#' # Three-year rolling prevalence
#' calc_prevalence(
#'   data = dt,
#'   denominator = "eligible",
#'   year_var = "year",
#'   outcome_var = "outcome",
#'   weight_var = "weight",
#'   rolling_year = 3
#' )
#'
#' # Three-year rolling prevalence accounting for PSU and strata
#' calc_prevalence(
#'   data = dt,
#'   denominator = "eligible",
#'   year_var = "year",
#'   outcome_var = "outcome",
#'   weight_var = "weight",
#'   psu = "psu",
#'   strata = "stratum",
#'   rolling_year = 3
#' )
#'
#' # Three-year rolling prevalence by region and gender
#' calc_prevalence(
#'   data = dt,
#'   denominator = "eligible",
#'   year_var = "year",
#'   outcome_var = "outcome",
#'   weight_var = "weight",
#'   psu = "psu",
#'   strata = "stratum",
#'   by = c("region", "gender"),
#'   rolling_year = 3
#' )
#'
#' @importFrom survey svydesign svyby svymean
#' @importFrom stats as.formula reformulate
#' @importFrom data.table rbindlist
#' @export
calc_prevalence <- function(data,
                            denominator,
                            year_var,
                            outcome_var,
                            weight_var = NULL,
                            psu = NULL,
                            strata = NULL,
                            by = NULL,
                            rolling_year = 3,
                            ci_level = 0.95) {

  # ------------------------------------------------------------------
  # Validate the main function arguments.
  # ------------------------------------------------------------------

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or data.table.")
  }

  if (!is.character(denominator) || length(denominator) != 1L) {
    stop("'denominator' must be a single character string.")
  }

  if (!is.character(year_var) || length(year_var) != 1L) {
    stop("'year_var' must be a single character string.")
  }

  if (!is.character(outcome_var) || length(outcome_var) != 1L) {
    stop("'outcome_var' must be a single character string.")
  }

  if (!is.null(weight_var) && 
        (!is.character(weight_var) || length(weight_var) != 1L)) {
    stop("'weight_var' must be a single character string.")
  }

  if (!is.null(psu) &&
      (!is.character(psu) || length(psu) != 1L)) {
    stop("'psu' must be NULL or a single character string.")
  }

  if (!is.null(strata) &&
      (!is.character(strata) || length(strata) != 1L)) {
    stop("'strata' must be NULL or a single character string.")
  }

  if (!is.null(by) &&
      (!is.character(by) || length(by) < 1L)) {
    stop("'by' must be NULL or a character vector of variable names.")
  }

  if (!is.numeric(rolling_year) ||
      length(rolling_year) != 1L ||
      is.na(rolling_year) ||
      rolling_year < 1 ||
      rolling_year != as.integer(rolling_year)) {
    stop("'rolling_year' must be a positive integer.")
  }

  rolling_year <- as.integer(rolling_year)

  if (!is.numeric(ci_level) ||
      length(ci_level) != 1L ||
      is.na(ci_level) ||
      ci_level <= 0 ||
      ci_level >= 1) {
    stop("'ci_level' must be a single number between 0 and 1.")
  }

  # ------------------------------------------------------------------
  # Check that all requested variables exist in the data.
  # ------------------------------------------------------------------

  variables <- unique(c(
    denominator,
    year_var,
    outcome_var,
#     weight_var,
    psu,
    strata,
    by
  ))

  missing_variables <- setdiff(
    variables,
    names(data)
  )

  if (length(missing_variables) > 0L) {
    stop(
      "The following variables are not present in 'data': ",
      paste(missing_variables, collapse = ", ")
    )
  }

  # ------------------------------------------------------------------
  # Check the year variable.
  # ------------------------------------------------------------------

  if (!is.numeric(data[[year_var]]) &&
      !is.integer(data[[year_var]])) {
    stop("'year_var' must contain numeric/integer calendar years.")
  }

  if (anyNA(data[[year_var]])) {
    stop("'year_var' contains missing values.")
  }

  # ------------------------------------------------------------------
  # Check that the denominator is binary.
  #
  # The denominator defines eligibility for the analysis population:
  #
  #   denominator = 1 -> included
  #   denominator = 0 -> excluded
  #
  # Missing denominator values are not allowed because it would be
  # unclear whether those observations belong to the denominator.
  # ------------------------------------------------------------------

  denominator_values <- unique(data[[denominator]])

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

  # ------------------------------------------------------------------
  # Restrict the data to the outcome-specific denominator population.
  #
  # This is deliberately done before constructing rolling rolling_years or
  # the survey design. Consequently, the denominator determines which
  # observations contribute to both the prevalence and its variance.
  # ------------------------------------------------------------------

  data <- data[
    data[[denominator]] == 1,
    ,
    drop = FALSE
  ]

  if (nrow(data) == 0L) {
    stop(
      "No observations remain after restricting to ",
      denominator,
      " == 1."
    )
  }

  # ------------------------------------------------------------------
  # Check that the outcome is binary within the eligible population.
  # ------------------------------------------------------------------

  outcome_values <- unique(
    data[[outcome_var]][!is.na(data[[outcome_var]])]
  )

  if (length(outcome_values) == 0L) {
    stop(
      "No non-missing observations remain for 'outcome_var' ",
      "within the denominator population."
    )
  }

  if (!all(outcome_values %in% c(0, 1))) {
    stop(
      "'outcome_var' must be coded as 0/1. Found values: ",
      paste(sort(outcome_values), collapse = ", ")
    )
  }

  # ------------------------------------------------------------------
  # Check that the survey weights are valid.
  #
  # Survey weights should be positive and finite. Zero or negative
  # weights are not appropriate for this survey design.
  # ------------------------------------------------------------------

  if (is.null(weight_var)){
    data$weight_var <- 1
    weight_var = "weight_var"
  }
  
  weights <- data[[weight_var]]

  if (anyNA(weights) || any(!is.finite(weights))) {
    stop(
      "'weight_var' contains missing, infinite, or non-finite values."
    )
  }

  if (any(weights <= 0)) {
    stop(
      "'weight_var' must contain positive values."
    )
  }

  # ------------------------------------------------------------------
  # Remove observations with missing outcome.
  #
  # The denominator restriction has already been applied above, so
  # missing outcomes are removed only from the eligible population.
  # ------------------------------------------------------------------

  data <- data[
    !is.na(data[[outcome_var]]),
    ,
    drop = FALSE
  ]

  if (nrow(data) == 0L) {
    stop(
      "No observations remain after removing missing values ",
      "from 'outcome_var'."
    )
  }

  # ------------------------------------------------------------------
  # Identify the calendar years available in the eligible population.
  # ------------------------------------------------------------------

  years <- sort(unique(data[[year_var]]))

  # The function requires at least 'rolling_year' distinct calendar years.
  if (length(years) < rolling_year) {
    stop(
      "The denominator population contains ", length(years),
      " unique year(s), but 'rolling_year' is ", rolling_year, "."
    )
  }

  # ------------------------------------------------------------------
  # Determine the end year for each rolling period.
  #
  # For rolling_year = 3:
  #
  #   2018-2020
  #   2019-2021
  #   2020-2022
  #
  # For rolling_year = 1:
  #
  #   2018-2018
  #   2019-2019
  #   2020-2020
  #
  # Only periods with the required number of consecutive calendar
  # years are created.
  # ------------------------------------------------------------------

  candidate_end_years <- years[
    seq.int(from = rolling_year, to = length(years))
  ]

  end_years <- candidate_end_years[
    vapply(
      candidate_end_years,
      function(end_year) {
        start_year <- end_year - rolling_year + 1L

        # Check that every calendar year in the requested interval
        # is represented in the data.
        all(
          seq.int(start_year, end_year) %in% years
        )
      },
      logical(1)
    )
  ]

  if (length(end_years) == 0L) {
    stop(
      "No complete ", rolling_year,
      "-year calendar rolling_years can be constructed from the available years."
    )
  }

  # ------------------------------------------------------------------
  # Create the data set containing all rolling periods.
  #
  # Rolling periods intentionally overlap. The same survey observation
  # may therefore occur in more than one period.
  # ------------------------------------------------------------------

  roll_data <- data.table::rbindlist(
    lapply(end_years, function(end_year) {

      start_year <- end_year - rolling_year + 1L

      # Select observations within the current calendar-year rolling_year.
      tmp <- data[
        data[[year_var]] >= start_year &
          data[[year_var]] <= end_year,
        ,
        drop = FALSE
      ]

      # Create a label identifying the rolling period.
      tmp$rolling_period <- sprintf(
        "%d-%d",
        start_year,
        end_year
      )

      tmp
    }),
    use.names = TRUE,
    fill = FALSE
  )

  # ------------------------------------------------------------------
  # Construct the PSU specification.
  #
  # When no PSU is supplied, ids = ~1 tells the survey package to treat
  # observations as independent sampling units.
  #
  # When a PSU variable is supplied, observations sharing the same PSU
  # are treated as belonging to the same sampling cluster.
  # ------------------------------------------------------------------

  ids_formula <- if (is.null(psu)) {
    ~1
  } else {
    stats::as.formula(
      paste0("~", psu)
    )
  }

  # ------------------------------------------------------------------
  # Construct the strata specification.
  #
  # NULL means that no stratification is specified.
  # ------------------------------------------------------------------

  strata_formula <- if (is.null(strata)) {
    NULL
  } else {
    stats::as.formula(
      paste0("~", strata)
    )
  }

  # ------------------------------------------------------------------
  # Construct the survey-weight specification.
  # ------------------------------------------------------------------

  weights_formula <- stats::as.formula(
    paste0("~", weight_var)
  )

  # ------------------------------------------------------------------
  # Create the survey design object.
  #
  # nest = TRUE allows the same PSU identifier to occur in different
  # strata. This is appropriate when PSU identifiers are only unique
  # within strata.
  #
  # If the survey's PSU identifiers are globally unique, nest = TRUE
  # does not generally change the interpretation.
  # ------------------------------------------------------------------

  design <- survey::svydesign(
    ids = ids_formula,
    strata = strata_formula,
    weights = weights_formula,
    data = roll_data,
    nest = TRUE
  )

  # ------------------------------------------------------------------
  # Define the variables used to form estimation domains.
  #
  # rolling_period is always included.
  #
  # If 'by' is supplied, estimates are calculated separately for each
  # combination of the requested subgroup variables.
  # ------------------------------------------------------------------

  by_vars <- c("rolling_period", by)

  by_formula <- stats::reformulate(by_vars)

  # ------------------------------------------------------------------
  # Calculate prevalence and confidence intervals.
  #
  # For a binary 0/1 outcome, the survey-weighted mean is equivalent
  # to prevalence.
  #
  # svyby() calculates the estimate separately within each domain and
  # uses the survey design to estimate the variance.
  # ------------------------------------------------------------------

  result <- survey::svyby(
    formula = stats::as.formula(
      paste0("~", outcome_var)
    ),
    by = by_formula,
    design = design,
    FUN = survey::svymean,
    vartype = "ci",
    level = ci_level,
    na.rm = TRUE
  )

  # ------------------------------------------------------------------
  # Return the prevalence estimates and confidence intervals.
  # ------------------------------------------------------------------

  result
}
