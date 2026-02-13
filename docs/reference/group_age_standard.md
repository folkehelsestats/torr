# Create Standard Demographic Age Groups

A convenience wrapper around
[`group_age()`](https://github.com/folkehelsestats/torr/reference/group_age.md)
that creates commonly used demographic age categories.

## Usage

``` r
group_age_standard(
  dt,
  var,
  type = "standard",
  new_var = NULL,
  copy = FALSE,
  missing_values = NULL,
  ...
)
```

## Arguments

- dt:

  data.table. The input data.table object.

- var:

  Character string. Name of the age variable.

- type:

  Character string. Type of age grouping. Options are:

  - "standard": 0-17, 18-34, 35-54, 55-74, 75+

  - "pediatric": 0-2, 3-5, 6-12, 13-17, 18+

  - "young30": 16-20, 21-25, 26-30, 31+

  - "young34": 16-20, 21-25, 26-30, 31-34, 35+

  - "geriatric": \<65, 65-74, 75-84, 85+

  - "working": \<18, 18-64, 65+

  - "unodc": 16-17, 18-24, 25-34, 35-64, 65+

  - "rusund": 16-24, 25-34, 35-44, 45-54, 55-64, 65-79

- new_var:

  Character string, optional. Name for the new variable.

- copy:

  Logical. Return a copy instead of modifying by reference?

- missing_values:

  Numeric vector, optional. Values that should be treated as missing
  (e.g., survey codes). These values will be converted to NA before
  categorization. Default is NULL.

- ...:

  Other arguments in
  [`group_age()`](https://github.com/folkehelsestats/torr/reference/group_age.md)

## Value

Modified data.table with age group variable added.

## Examples

``` r
library(data.table)

# Create sample data
dt <- data.table(
  id = 1:100,
  age = sample(18:80, 100, replace = TRUE),
  gender = sample(c("M", "F"), 100, replace = TRUE)
)

# Create standard demographic groups
dt_demo <- group_age_standard(dt, "age", type = "standard")

# Create working-age focused groups
dt_work <- group_age_standard(dt, "age", type = "working", copy = TRUE)
```
