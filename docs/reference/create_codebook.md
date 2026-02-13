# Create an interactive codebook using DT

This function generates an interactive HTML table (codebook) for a
dataset, showing variable names, labels, and value labels extracted from
attributes.

## Usage

``` r
create_codebook(d, page = 25, save = FALSE)
```

## Arguments

- d:

  A data frame, typically imported with `haven` or similar, where
  variables have `label` and `labels` attributes.

- page:

  Integer. Number of rows to display per page in the interactive table.
  Default is 25.

- save:

  Logical value. Default is FALSE

## Value

A [`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) object
that can be rendered in R Markdown or Shiny or a stand alone HTML file.

## Details

The value labels are displayed on separate lines using HTML `<br>` tags.
To ensure these tags are rendered correctly, the argument
`escape = FALSE` is passed to
[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html). Without
this, HTML tags would be shown as plain text instead of creating line
breaks.

The table includes:

- Column filters for easy searching.

- Export buttons (Copy, CSV, Excel, PDF, Print).

## Examples

``` r
if (FALSE) { # \dontrun{
library(haven)
df <- read_dta("yourfile.dta")
create_codebook(df)
} # }
```
