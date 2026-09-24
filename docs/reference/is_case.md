# Create a binary indicator from one or more source variables

Creates a new binary variable based on whether one or more source
variables equal a specified value. Missing values are treated as
non-matches.

## Usage

``` r
is_case(dt, varfrom, varto, val = 1)
```

## Arguments

- dt:

  A `data.table`.

- varfrom:

  A character vector containing one or more source column names.

- varto:

  A character string specifying the name of the output column.

- val:

  A value used to identify cases. Defaults to `1`.

## Value

A copy of `dt` with an additional binary column specified by `varto`.

## Details

For a single source variable, the output is:

- 1 if the variable equals `val`

- 0 if the variable does not equal `val`

- 0 if the variable is `NA`

For multiple source variables, the output is:

- 1 if at least one variable equals `val`

- 0 otherwise

## Examples

``` r
library(data.table)

dt <- data.table(
  var1 = c(1, 0, NA, 0),
  var2 = c(0, 1, 1, NA)
)

# Single source variable
is_case(dt, "var1", "case1")
#> Error in is_case(dt, "var1", "case1"): could not find function "is_case"

# Multiple source variables
is_case(dt, c("var1", "var2"), "case2")
#> Error in is_case(dt, c("var1", "var2"), "case2"): could not find function "is_case"
```
