
#' Create an interactive codebook using DT
#'
#' This function generates an interactive HTML table (codebook) for a dataset,
#' showing variable names, labels, and value labels extracted from attributes.
#'
#' @param d A data frame, typically imported with `haven` or similar, where
#' variables have `label` and `labels` attributes.
#' @param page Integer. Number of rows to display per page in the interactive table. Default is 25.
#' @param save Logical value. Default is FALSE
#' @param file Character scalar specifying the path and file name where the
#'   output will be saved. The file name must end with `.html`.
#' @return A `DT::datatable` object that can be rendered in R Markdown or Shiny or a stand alone HTML file.
#'
#' @details
#' The value labels are displayed on separate lines using HTML `<br>` tags.
#' To ensure these tags are rendered correctly, the argument `escape = FALSE`
#' is passed to `DT::datatable()`. Without this, HTML tags would be shown as
#' plain text instead of creating line breaks.
#'
#' The table includes:
#' \itemize{
#'   \item Column filters for easy searching.
#'   \item Export buttons (Copy, CSV, Excel, PDF, Print).
#' }
#'
#' @examples
#' \dontrun{
#' library(haven)
#' df <- read_dta("yourfile.dta")
#' create_codebook(df)
#' }
#' @export
create_codebook <- function(d, page = 25, save = FALSE, file = NULL) {
  df <- extract_attr(d)

  if (!is.null(path)){
    assert_parent_dir_exists(path)
    save = TRUE
  } else {
    file = paste0(deparse(substitute(d)), ".html")
  }
  
  code <- DT::datatable(df, filter = "top",
                        escape = FALSE,
                        extensions = 'Buttons',
                        rownames = FALSE,
                        options = list(
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                          pageLength = page
                        ))

  if (save){
    htmlwidgets::saveWidget(code,
                            file = file,
                            selfcontained = TRUE)
  } else {
    return(code)
  }
}

#' Extract variable metadata from a dataset
#'
#' This helper function extracts variable names, labels, and value labels
#' from a dataset where these are stored as attributes (e.g., `haven` imported data).
#'
#' @param d A data frame with attributes `label` and `labels` for each variable.
#'
#' @return A data.table with columns:
#' \describe{
#'   \item{name}{Variable name}
#'   \item{label}{Variable label (question text)}
#'   \item{value_labels}{Value labels formatted with HTML line breaks}
#' }
#'
#' @details
#' The function converts value labels into a single string separated by `<br>`
#' tags for proper display in HTML tables. For example:
#' \code{c("1 Yes", "2 No")} becomes \code{"1 Yes<br>2 No"}.
#'
#' @examples
#' \dontrun{
#' df <- read_spss("yourfile.sav")
#' extract_attr(df)
#' }
#'
#' @keywords internal
#' @noRd
extract_attr <- function(d) {
  vars <- names(d)
  labels <- sapply(d, function(x) attr(x, "label"))
  value_labels <- sapply(d, function(x) {
    lbls <- attr(x, "labels")
    if (!is.null(lbls)) {
      paste0(lbls, " ", names(lbls), collapse = "<br>")
    } else {
      ""
    }
  })

  data.table::data.table(name = vars,
                         label = labels,
                         value_labels = value_labels
                         )
}


#' Assert that the output directory exists
#'
#' Internal helper that validates the parent directory of an output file.
#' The output file itself does not need to exist. This function only
#' verifies that its destination directory exists.
#'
#' Relative paths are normalized before validation, making the function
#' safe to use with both Windows and Unix-style file paths.
#'
#' @param path Character scalar containing the path to an output file.
#'
#' @return Invisibly returns `TRUE` when the parent directory exists.
#'
#' @keywords internal
#' @noRd
assert_parent_dir_exists <- function(path) {

  # Ensure a single non-empty character path was supplied
  if (
    missing(path) ||
    is.null(path) ||
    !is.character(path) ||
    length(path) != 1L ||
    !nzchar(path)
  ) {
    stop(
      "`path` must be a non-empty character string.",
      call. = FALSE
    )
  }

  # Extract the parent directory from the supplied file path
  dir_path <- base::dirname(path)

  # Normalize the directory path to handle:
  # - Relative paths (., ..)
  # - Windows path separators (\)
  # - Mixed path formats
  dir_path <- base::normalizePath(
    path = dir_path,
    winslash = "/",
    mustWork = FALSE
  )

  # Verify that the destination directory exists
  if (!base::dir.exists(dir_path)) {
    stop(
      sprintf(
        "Output directory does not exist: '%s'.",
        dir_path
      ),
      call. = FALSE
    )
  }

  # Extract and validate the file extension
  if (tolower(tools::file_ext(path)) != "html") {
    stop(
      sprintf(
        "Expected an '.html' file, but got: '%s'.",
        path
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
