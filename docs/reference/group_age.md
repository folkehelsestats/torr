# Categorize Age Variable into Groups

This function creates age groups from a continuous age variable using
specified breaks and labels. It's designed to work with data.table
objects and provides flexible options for defining age categories
commonly used in demographic analysis.

## Usage

``` r
group_age(
  dt,
  var,
  breaks,
  labels = NULL,
  new_var = NULL,
  right = FALSE,
  include.lowest = TRUE,
  validate = TRUE,
  copy = FALSE,
  missing_values = NULL
)
```

## Arguments

- dt:

  data.table. The input data.table object containing the age variable.

- var:

  Character string. Name of the age variable to be categorized.

- breaks:

  Numeric vector. Break points for age categories. Should include the
  minimum value to ensure all ages are captured.

- labels:

  Character vector, optional. Labels for the age groups. If NULL,
  default labels will be generated automatically. Length should be one
  less than the number of breaks. Default is NULL.

- new_var:

  Character string, optional. Name of the new categorical variable to be
  created. If NULL, defaults to "`_group". Default is NULL.`

- right:

  Logical. Should intervals be closed on the right (and open on the
  left) or vice versa? Default is FALSE.

- include.lowest:

  Logical. Should the lowest break point be included in the first
  interval? Default is TRUE.

- validate:

  Logical. Should input validation be performed? Default is TRUE.

- copy:

  Logical. Should a copy of the data.table be returned instead of
  modifying in place? If TRUE then object name should have different
  name. Default is FALSE.

- missing_values:

  Numeric vector, optional. Values that should be treated as missing
  (e.g., survey codes like 999 for "don't know", 998 for "no answer").
  These values will be converted to NA before categorization. Default is
  NULL.

## Value

The modified data.table with the new age group variable added. If
copy=TRUE, returns a new data.table; otherwise modifies the input
data.table by reference.

## Details

The function uses base R's [`cut()`](https://rdrr.io/r/base/cut.html)
function internally to create the age categories. Common age groupings
in demographic analysis include:

- Pediatric: 0-18 years

- Young adult: 19-30 years

- Middle age: 31-50 years

- Older adult: 51+ years

The breaks vector should always include the minimum expected age (often
0) and a maximum value that exceeds the highest age in your data.

## Missing Value Handling

Survey data often uses special numeric codes to represent different
types of missing responses:

- 999: "Don't know"

- 998: "Prefer not to answer"

- 997: "Not applicable"

- -1, -9: Various missing codes

The `missing_values` parameter allows you to specify these codes, which
will be converted to NA before age categorization. This ensures that
survey response codes don't interfere with legitimate age values and are
properly treated as missing data in the resulting age groups.

When validate=TRUE, the function checks for:

- Existence of the specified age variable

- Numeric age variable

- Proper breaks vector (numeric, sorted, minimum length 2)

- Matching labels length (if provided)

- Coverage of all valid age values by the breaks (excluding missing
  codes)

- Valid missing_values parameter (if provided)

## See also

[`cut`](https://rdrr.io/r/base/cut.html) for the underlying
categorization function.
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) for
data.table operations.

## Examples

``` r
library(data.table)
#> Warning: package 'data.table' was built under R version 4.5.2

# Create sample data
dt <- data.table(
  id = 1:100,
  age = sample(18:80, 100, replace = TRUE),
  gender = sample(c("M", "F"), 100, replace = TRUE)
)

# Basic usage with automatic labels
age_breaks <- c(0, 18, 30, 50, 100)
dt_grouped <- group_age(dt, "age", breaks = age_breaks, copy = TRUE)

# With custom labels
age_labels <- c("Youth", "Young Adult", "Middle Age", "Senior")
dt_custom <- group_age(dt, "age",
                       breaks = age_breaks,
                       labels = age_labels,
                       new_var = "age_category",
                       copy = TRUE)

# With missing value codes (common in surveys)
dt_survey <- data.table(
  id = 1:100,
  age = c(sample(18:80, 90, replace = TRUE), rep(999, 5), rep(998, 5))
)

# Handle survey missing codes
dt_clean <- group_age(dt_survey, "age",
                      breaks = c(0, 18, 30, 50, 100),
                      labels = c("Youth", "Young Adult", "Middle Age", "Senior"),
                      missing_values = c(998, 999),  # "no answer", "don't know"
                      copy = TRUE)
#> Found 10 observations with missing value codes that will be converted to NA.

# Check the result
table(dt_clean$age_group, useNA = "ifany")
#> 
#>       Youth Young Adult  Middle Age      Senior        <NA> 
#>           0          16          26          48          10 
```
