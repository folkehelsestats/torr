# Calculate prevalence, confidence intervals, significance tests and trend

Calculates survey-weighted prevalence estimates and confidence intervals
for a binary outcome over single years, rolling multi-year periods or
user-defined groups of years, and (optionally) tests whether the
prevalence differs significantly between periods or between subgroups,
and whether there is a linear trend over time. Different analysis
populations can be specified for different outcomes through the
`denominator` variable.

## Usage

``` r
calc_prevalence(
  data,
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
  trend = TRUE
)
```

## Arguments

- data:

  A data frame or data.table containing the survey data.

- denominator:

  Character string giving the name of a binary denominator/eligibility
  variable. Observations with `denominator == 1` are included in the
  analysis population; observations with `denominator == 0` are
  excluded. The denominator variable therefore defines the population to
  which the prevalence estimate applies.

- year_var:

  Character string giving the name of the variable containing the
  calendar year (whole numbers).

- outcome_var:

  Character string giving the name of the binary outcome variable. The
  outcome must be coded as `0` and `1`, where `1` indicates the outcome
  of interest.

- weight_var:

  Optional character string giving the name of the survey weight
  variable. If `NULL`, an unweighted prevalence is calculated. Default
  is `NULL`.

- psu:

  Optional character string giving the name of the primary sampling unit
  (PSU) variable. If `NULL` (the default), every observation is treated
  as its own sampling unit.

- strata:

  Optional character string giving the name of the survey stratum
  variable. If `NULL` (the default), no stratification is specified.

- by:

  Optional character vector giving the names of variables defining
  subgroups for which prevalence should be calculated. If `NULL`,
  estimates are calculated for the entire analysis population within
  each period.

- rolling_year:

  Positive integer specifying the number of calendar years included in
  each rolling period. The default is `1`, i.e. prevalence is calculated
  separately for each year. Use e.g. `rolling_year = 3` for three-year
  rolling periods. Ignored (with a warning if it is not `1`) when
  `periods` is supplied.

- periods:

  Optional named list defining your own periods, instead of rolling
  periods. Each element is a vector of calendar years and the element
  name is the label used for that period in the output, e.g.
  `list("2013-2015" = 2013:2015, "2016-2025" = 2016:2025)`. The periods
  are kept in the order you give them, which matters for
  `compare_to = "first"`, `compare_to = "previous"` and the trend.
  Periods may have different lengths and may overlap. If `NULL` (the
  default), rolling periods defined by `rolling_year` are used. No
  recoding of the year variable is needed.

- ci_level:

  Numeric value between 0 and 1 specifying the confidence level for the
  confidence intervals. The default is `0.95`. The significance level
  used in the tests is `alpha = 1 - ci_level`.

- digits:

  Number of digits used when rounding the results (percentages and
  percentage-point differences). Default is `1`. P-values are not
  rounded.

- test:

  Logical. If `TRUE` (the default), significance tests between periods
  or subgroups are calculated and returned in `$tests`. If `FALSE`,
  `$tests` is `NULL`.

- compare_var:

  Character string giving the dimension along which prevalence is
  compared. Either `"rolling_period"` (the default; compare periods,
  i.e. change over time) or one of the variables in `by` (compare
  subgroups). Comparisons are always made *within* the levels of all
  other variables in `by`. For example, with
  `by = c("gender", "region")` and `compare_var = "gender"`, men and
  women are compared within each region and period. (The period variable
  is always called `rolling_period` in the output, also when you define
  the periods with `periods`.)

- compare_to:

  Defines the reference for the comparisons. One of:

  - `"first"` (the default): every level is compared with the first
    level (e.g. the earliest period).

  - `"previous"`: every level is compared with the level immediately
    before it (e.g. the previous period).

  - A character string giving a specific level of `compare_var` (e.g.
    `"2013-2015"`, a label from `periods`, or `"Female"`): every other
    level is compared with that level.

- p_adjust:

  Method for adjusting p-values for multiple comparisons. Any of
  [stats::p.adjust.methods](https://rdrr.io/r/stats/p.adjust.html), e.g.
  `"none"` (the default), `"holm"`, `"bonferroni"` or `"BH"`. The
  adjustment is applied across all comparisons in `$tests` (and
  separately across the trend tests in `$trend`).

- trend:

  Logical. If `TRUE` (the default), a linear trend test over the periods
  is calculated and returned in `$trend`, together with the fitted trend
  line and its confidence interval in `$fitted`. The trend is estimated
  separately for each combination of the `by` variables. At least three
  periods are required, so use `trend = FALSE` when you only define two
  periods with `periods`.

## Value

A named list with four elements:

- `estimates`:

  A data.table with one row per period (and per combination of the `by`
  variables) and the columns `rolling_period`, the `by` variables,
  `prevalence` (percent), `se` (percentage points), `ci_lower`,
  `ci_upper` (percent) and `n` (unweighted number of observations).

- `tests`:

  A data.table with one row per comparison (or `NULL` if `test = FALSE`
  or there is nothing to compare). Columns: the variables held fixed in
  the comparison (if any), `reference` and `compared` (the levels being
  compared), `prev_ref` and `prev_cmp` (prevalence in percent), `diff`
  (`prev_cmp - prev_ref` in percentage points), `diff_se`,
  `diff_ci_lower`, `diff_ci_upper`, `z`, `p_value`, `p_adjusted` and
  `significant` (`TRUE` if `p_adjusted < 1 - ci_level`).

- `trend`:

  A data.table with one row per combination of the `by` variables (or
  `NULL` if `trend = FALSE` or fewer than three periods are available).
  Columns: the `by` variables, `first_period`, `last_period`,
  `n_periods`, `slope` (average change in percentage points per year),
  `slope_se`, `slope_ci_lower`, `slope_ci_upper`, `z`, `p_value`,
  `p_adjusted`, `significant` and `direction` (`"increasing"`,
  `"decreasing"` or `"no significant trend"`).

- `fitted`:

  A data.table with the fitted trend line, one row per period (and per
  combination of the `by` variables), or `NULL` if `trend = FALSE` or no
  trend could be estimated. Columns: the `by` variables,
  `rolling_period`, `time` (the mean year of the period, used as the x
  value), `prevalence` (the observed estimate, percent), `fitted` (the
  value on the trend line, percent) and `lwr`, `upr` (the confidence
  interval for the fitted line, percent).

## Details

**Analysis population.** The `denominator` argument defines the analysis
population for the selected outcome. Only observations with
`denominator == 1` are used. The restriction is applied before missing
outcomes are removed, periods are constructed and the survey design is
created, so the denominator defines the population from which both the
prevalence and its variance are estimated. Missing values in
`denominator` cause an error. Observations with missing `outcome_var`
are removed.

**Survey design.** The design is constructed with
[`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
(with `nest = TRUE`). Estimates and variances come from
[`survey::svyby()`](https://rdrr.io/pkg/survey/man/svyby.html) and
[`survey::svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html).
When PSU and/or strata information are available they should be supplied
so that the variance estimator accounts for the complex sampling design.

**Rolling periods.** With `rolling_year = 3`, the estimate for 2022 uses
observations from 2020 through 2022. Only periods in which all calendar
years are present in the data are created. Periods overlap, so one
observation can contribute to several periods.

**Custom periods.** With `periods` you choose the groups of years
yourself, for example to compare a baseline period with all later years.
All observations from the listed years are pooled into one estimate for
that period. Years listed in `periods` that are not in the (eligible)
data give a warning; a period without any observations gives an error.
Because the pooled estimate for a long period is a weighted average over
its years, it is only comparable with another period if the survey
weights are comparable across the years.

**Significance tests.** For each comparison the difference in prevalence
between two estimates, \\\hat{p}\_2 - \hat{p}\_1\\, is tested with a
Wald z-test: \$\$ z = \frac{\hat{p}\_2 -
\hat{p}\_1}{\sqrt{V(\hat{p}\_1) + V(\hat{p}\_2) -
2\\\mathrm{Cov}(\hat{p}\_1, \hat{p}\_2)}}. \$\$ The covariance is taken
from the survey design (via `svyby(..., covmat = TRUE)`). This matters
for overlapping periods (rolling periods or overlapping `periods`),
because they share observations and their estimates are therefore
correlated; ignoring the covariance would give wrong standard errors.
Each observation is kept as one sampling unit (or the PSU, if supplied)
across all periods it appears in, which is what allows the covariance to
be estimated. Confidence intervals for the differences use the same
normal approximation and the level given by `ci_level`. The normal
approximation is also what
[`stats::confint()`](https://rdrr.io/r/stats/confint.html) uses for
`svyby` objects, so the tests and the confidence intervals in
`$estimates` are consistent.

**Trend test.** The trend is the slope of a straight line fitted to the
prevalence estimates against the midpoint year of each period (the mean
of the years in the period), so `slope` is the average change in
percentage points per year. The line is fitted by generalized least
squares using the full covariance matrix of the estimates from the
survey design, which accounts for the overlap between periods (ordinary
regression on overlapping estimates would treat them as independent and
give far too small standard errors). The slope is tested with a Wald
z-test. This is a test of a *linear* trend; a non-linear development
(e.g. flat and then rising) can still give a significant slope, so
inspect the estimates as well. The trend is a separate question from the
pairwise comparisons controlled by `compare_var`/`compare_to`.

**Fitted trend line.** `$fitted` contains the value of the same GLS line
at each period, \\\hat{y}\_t = \hat{\beta}\_0 + \hat{\beta}\_1 t\\, with
a confidence interval \\\hat{y}\_t \pm z \cdot SE(\hat{y}\_t)\\, where
\\SE(\hat{y}\_t)\\ comes from the covariance matrix of the estimated
coefficients. This is the confidence interval for the *line* (the
average development), in the same way as
`predict(lm(...), interval = "confidence")`; it is not a prediction
interval for a single new estimate and it is not the same as the
confidence interval of the observed prevalence in `$estimates`. Because
the line is not restricted, `lwr` and `upr` can fall below 0 or above
100 for very low or very high prevalence. The fitted values use exactly
the same fit as the slope in `$trend`, so the plotted line and the
reported slope always agree. Unlike
[`lm()`](https://rdrr.io/r/stats/lm.html), the fit uses the survey
covariance matrix, so the line is not the same as an ordinary
least-squares line through the period estimates.

**Interpretation.** `significant = TRUE` means that the difference is
statistically significant at level `1 - ci_level` after the chosen
multiple-comparison adjustment. A statistically significant difference
is not necessarily a large or important one; consider the size of `diff`
and its confidence interval. When many comparisons are made, consider
using `p_adjust`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Annual prevalence, change over time compared with the first year
res <- calc_prevalence(
  data = dt,
  denominator = "eligible",
  year_var = "year",
  outcome_var = "outcome",
  weight_var = "weight",
  rolling_year = 1
)
res$estimates
res$tests
res$trend
res$fitted   # fitted trend line with confidence interval, e.g. for plotting

# Three-year rolling prevalence with PSU and strata, each period
# compared with the previous one, Holm-adjusted p-values
calc_prevalence(
  data = dt,
  denominator = "eligible",
  year_var = "year",
  outcome_var = "outcome",
  weight_var = "weight",
  psu = "psu",
  strata = "stratum",
  rolling_year = 3,
  compare_to = "previous",
  p_adjust = "holm"
)

# Custom periods: 2013-2015 compared with 2016-2025.
# No new year column is needed. Only two periods, so no trend.
calc_prevalence(
  data = dt,
  denominator = "eligible",
  year_var = "year",
  outcome_var = "outcome",
  weight_var = "weight",
  periods = list(
    "2013-2015" = 2013:2015,
    "2016-2025" = 2016:2025
  ),
  trend = FALSE
)

# Custom periods by gender: three periods, each compared with the
# baseline period, with a trend over the three periods
calc_prevalence(
  data = dt,
  denominator = "eligible",
  year_var = "year",
  outcome_var = "outcome",
  weight_var = "weight",
  by = "gender",
  periods = list(
    "2013-2016" = 2013:2016,
    "2017-2020" = 2017:2020,
    "2021-2025" = 2021:2025
  ),
  compare_to = "2013-2016"
)

# Compare genders within each rolling period
calc_prevalence(
  data = dt,
  denominator = "eligible",
  year_var = "year",
  outcome_var = "outcome",
  weight_var = "weight",
  psu = "psu",
  strata = "stratum",
  by = c("region", "gender"),
  rolling_year = 3,
  compare_var = "gender",
  compare_to = "Male"
)

# Estimates only, no tests and no trend
calc_prevalence(
  data = dt, denominator = "eligible", year_var = "year",
  outcome_var = "outcome", weight_var = "weight",
  test = FALSE, trend = FALSE
)$estimates
} # }
```
