# Create Population Indicators for LTP and LYP Variables

This function generates binary population indicators based on LTP
(Lifetime prevalence) and/or LYP (Last Year Prevalence) variables. It
creates new columns in the dataset with values of 1 (in population) or 0
(not in population) based on specified conditions.

## Usage

``` r
create_narko_pop(d, types = c("ltp", "lyp"), vars, val, ans = "ans1")
```

## Arguments

- d:

  A data.table or data.frame. The input dataset.

- types:

  A character vector specifying which population types to calculate.
  Options are "ltp" and/or "lyp". Default is `c("ltp", "lyp")`.

- vars:

  A named or unnamed character vector of variable names corresponding to
  the types. If named, names should match the types. If unnamed, order
  should match the order in `types`.

- val:

  A character string used as a suffix for the created variable names.
  New variables will be named `ltpPop_<val>` and/or `lypPop_<val>`.

- ans:

  A character string specifying the name of the answer/response column
  in the dataset for ltp drug question. Default is `"ans1"`.

## Value

A data.table with new population indicator columns added. The original
data.table is not modified (a copy is made internally).

## Details

The function creates population indicators based on the following logic:

- For LTP: Value is 1 if the LTP variable is 1 or 2, OR if LTP is
  missing and the answer variable ans1 is equals 2. Otherwise 0.

- For LYP: Value is 1 if the LYP variable is 1 or 2, OR if LYP is
  missing and LTP equals 2, OR if LYP is missing and the answer variable
  ans2 is equals 2. Otherwise 0.

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)

# Create sample data
dt <- data.table(
  id = 1:5,
  ltp_var = c(1, 2, NA, 3, NA),
  lyp_var = c(1, NA, 2, NA, NA),
  ans1 = c(1, 1, 2, 1, 2)
)

# Calculate both LTP and LYP populations
result <- narko_pop(
  d = dt,
  types = c("ltp", "lyp"),
  vars = c("ltp_var", "lyp_var"),
  val = "2024"
)

# Calculate only LTP population
result_ltp <- narko_pop(
  d = dt,
  types = "ltp",
  vars = "ltp_var",
  val = "2024"
)
} # }
```
