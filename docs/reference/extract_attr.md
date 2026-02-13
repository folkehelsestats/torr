# Extract variable metadata from a dataset

This helper function extracts variable names, labels, and value labels
from a dataset where these are stored as attributes (e.g., `haven`
imported data).

## Usage

``` r
extract_attr(d)
```

## Arguments

- d:

  A data frame with attributes `label` and `labels` for each variable.

## Value

A data.table with columns:

- name:

  Variable name

- label:

  Variable label (question text)

- value_labels:

  Value labels formatted with HTML line breaks

## Details

The function converts value labels into a single string separated by
`<br>` tags for proper display in HTML tables. For example:
`c("1 Yes", "2 No")` becomes `"1 Yes<br>2 No"`.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- read_spss("yourfile.sav")
extract_attr(df)
} # }
```
