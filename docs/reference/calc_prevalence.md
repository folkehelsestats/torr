# Calculate prevalence and confidence intervals

Calculates survey-weighted prevalence estimates and confidence intervals
for a binary outcome over single-year or rolling multi-year periods.
Different analysis populations can be specified for different outcomes
through the `denominator` variable.

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
  ci_level = 0.95,
  digits = 1
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
  calendar year.

- outcome_var:

  Character string giving the name of the binary outcome variable. The
  outcome must be coded as `0` and `1`, where `1` indicates the outcome
  of interest.

- weight_var:

  Character string giving the name of the survey weight variable. If
  NULL then run an uweighted prevalence. Default is NULL.

- psu:

  Optional character string giving the name of the primary sampling unit
  (PSU) variable. If `NULL` (the default), `ids = ~1` is used and no
  clustering is specified.

- strata:

  Optional character string giving the name of the survey stratum
  variable. If `NULL` (the default), no stratification is specified.

- by:

  Optional character vector giving the names of variables defining
  subgroups for which prevalence should be calculated. If `NULL`,
  estimates are calculated for the entire analysis population within
  each rolling period.

- rolling_year:

  Positive integer specifying the number of calendar years included in
  each rolling period. The default is `3`. Use `rolling_year = 1` to
  calculate prevalence separately for each year.

- ci_level:

  Numeric value between 0 and 1 specifying the confidence level for the
  confidence intervals. The default is `0.95`.

- digits:

  Number of digits to show for the prevalence. Default is 1.

## Value

A data frame containing the prevalence estimates and confidence
intervals produced by
[`survey::svyby()`](https://rdrr.io/pkg/survey/man/svyby.html). The
prevalence estimate is the survey-weighted mean of `outcome_var`. For a
0/1 outcome this is equivalent to prevalence. Confidence interval
columns are typically named `ci.2.5` and `ci.97.5` for a \\95\\\\
confidence interval. The results are in precentage.

## Details

The `denominator` argument defines the analysis population for the
selected outcome. Only observations with `denominator == 1` are used in
the prevalence calculation. This allows different outcomes to have
different eligible populations. For example, one outcome may be
applicable to the entire survey population while another may only be
applicable to a particular age group or other eligible population.

The denominator variable is applied before missing outcomes are removed,
rolling periods are constructed, and the survey design is created.
Therefore, the denominator defines the population from which the
prevalence and its variance are estimated.

The survey design is constructed using
[`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html).
If `psu` is not supplied, `ids = ~1` is used, treating observations as
independent sampling units. If `psu` is supplied, it is used as the
primary sampling unit. If `strata` is supplied, it is used to account
for stratification.

The confidence intervals are based on the survey-design variance
estimator used by
[`survey::svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
and [`survey::svyby()`](https://rdrr.io/pkg/survey/man/svyby.html). They
are therefore different from confidence intervals calculated using an
effective sample size, such as \$\$ SE =
\sqrt{p(1-p)/n\_{\mathrm{eff}}}, \$\$ where \$\$ n\_{\mathrm{eff}} =
\frac{(\sum w_i)^2}{\sum w_i^2}. \$\$

When PSU and/or strata information are available from the original
survey design, they should be supplied because they allow the variance
estimator to account for the complex sample design.

Rolling rolling_years are defined using calendar years. For example,
with `rolling_year = 3`, the estimate for 2022 uses observations from
2020 through 2022. With `rolling_year = 1`, each year is treated as its
own estimation period.

Note that rolling periods overlap. For example, an observation from 2020
can contribute to the 2018–2020, 2019–2021, and 2020–2022 estimates. The
estimates from overlapping periods should therefore not be treated as
statistically independent.

Missing values in `denominator` are not treated as eligible and cause an
error because the analysis population cannot be determined. Observations
with missing values in `outcome_var` are removed because they cannot
contribute to the prevalence estimate.

The outcome must be coded as 0/1. For such an outcome, the
survey-weighted mean is the prevalence of the outcome.

## Examples

``` r
# Annual prevalence for an outcome with a specific denominator
calc_prevalence(
  data = dt,
  denominator = "eligible",
  year_var = "year",
  outcome_var = "outcome",
  weight_var = "weight",
  rolling_year = 1
)
#> Error in calc_prevalence(data = dt, denominator = "eligible", year_var = "year",     outcome_var = "outcome", weight_var = "weight", rolling_year = 1): 'data' must be a data.frame or data.table.

# Three-year rolling prevalence
calc_prevalence(
  data = dt,
  denominator = "eligible",
  year_var = "year",
  outcome_var = "outcome",
  weight_var = "weight",
  rolling_year = 3
)
#> Error in calc_prevalence(data = dt, denominator = "eligible", year_var = "year",     outcome_var = "outcome", weight_var = "weight", rolling_year = 3): 'data' must be a data.frame or data.table.

# Three-year rolling prevalence accounting for PSU and strata
calc_prevalence(
  data = dt,
  denominator = "eligible",
  year_var = "year",
  outcome_var = "outcome",
  weight_var = "weight",
  psu = "psu",
  strata = "stratum",
  rolling_year = 3
)
#> Error in calc_prevalence(data = dt, denominator = "eligible", year_var = "year",     outcome_var = "outcome", weight_var = "weight", psu = "psu",     strata = "stratum", rolling_year = 3): 'data' must be a data.frame or data.table.

# Three-year rolling prevalence by region and gender
calc_prevalence(
  data = dt,
  denominator = "eligible",
  year_var = "year",
  outcome_var = "outcome",
  weight_var = "weight",
  psu = "psu",
  strata = "stratum",
  by = c("region", "gender"),
  rolling_year = 3
)
#> Error in calc_prevalence(data = dt, denominator = "eligible", year_var = "year",     outcome_var = "outcome", weight_var = "weight", psu = "psu",     strata = "stratum", by = c("region", "gender"), rolling_year = 3): 'data' must be a data.frame or data.table.
```
