# Create Cannabis Population Indicators

Creates binary population indicators based on lifetime, last year,
and/or last month substance use variables. The function generates
columns indicating whether individuals have ever used, used in the past
year, or used in the past month. Users can specify which indicators to
create.

## Usage

``` r
create_cann_pop(dt, types = c("ltp", "lyp", "lmp"), vars, value = "cannabis")
```

## Arguments

- dt:

  A data.table or data.frame containing the substance use variables.

- types:

  A character vector specifying which indicators to create. Must contain
  one or more of: "ltp" (lifetime), "lyp" (last year), "lmp" (last
  month). Default is `c("ltp", "lyp", "lmp")` to create all three
  indicators. The order and content of `types` must match the order and
  content of `vars`.

- vars:

  A character vector containing variable names corresponding to `types`.

  - If types = "ltp", provide 1 variable (lifetime use)

  - If types = c("ltp", "lyp"), provide 2 variables (lifetime, last
    year)

  - If types = c("ltp", "lyp", "lmp"), provide 3 variables (lifetime,
    last year, last month)

  Variables should be coded where 1-2 indicate use.

- value:

  A character string to use as a suffix for the created variables.
  Default is "cannabis".

## Value

A data.table with additional columns based on `types`:

- ltpPop\_{value}:

  Lifetime population indicator (1 = ever used, 0 = never)

- lypPop\_{value}:

  Last year population indicator (1 = used in past year, 0 = no)

- lmpPop\_{value}:

  Last month population indicator (1 = used in past month, 0 = no)

## Details

The function creates cascading indicators:

- **Lifetime (ltp):** 1 if lifetime use variable is 1 or 2

- **Last year (lyp):** 1 if last year variable is 1-2, OR if lifetime
  variable is 2

- **Last month (lmp):** 1 if last month variable is 1-2, OR if last year
  variable is 2, OR if lifetime variable is 2

The cascading logic means:

- To create "lyp", you must also include "ltp" in types

- To create "lmp", you must also include both "ltp" and "lyp" in types

## Note

The input data.table is copied to avoid modifying by reference. Missing
values are handled as FALSE (coded as 0) in all conditions.

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)

# Create sample data
dt <- data.table(
  id = 1:5,
  lifetime_use = c(1, 2, 3, 1, NA),
  year_use = c(1, 2, 3, 2, 1),
  month_use = c(1, 3, 3, 1, 2)
)

# Create all three indicators (default)
result <- create_cann_pop(
  dt = dt,
  vars = c("can1", "can6", "can10"),
  value = "cannabis"
)

# Create only lifetime indicator
result <- create_cann_pop(
  dt = dt,
  types = "ltp",
  vars = "lifetime_use",
  value = "cannabis"
)

# Create lifetime and last year indicators
result <- create_cann_pop(
  dt = dt,
  types = c("ltp", "lyp"),
  vars = c("lifetime_use", "year_use"),
  value = "cannabis"
)

# With different substance
result <- create_cann_pop(
  dt = dt,
  types = c("ltp", "lyp", "lmp"),
  vars = c("can1", "can6", "can10"),
  value = "mdma"
)
} # }
```
