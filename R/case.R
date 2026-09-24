#' Create a binary indicator from one or more source variables
#'
#' Creates a new binary variable based on whether one or more source
#' variables equal a specified value. Missing values are treated as
#' non-matches.
#'
#' For a single source variable, the output is:
#' \itemize{
#'   \item 1 if the variable equals `val`
#'   \item 0 if the variable does not equal `val`
#'   \item 0 if the variable is `NA`
#' }
#'
#' For multiple source variables, the output is:
#' \itemize{
#'   \item 1 if at least one variable equals `val`
#'   \item 0 otherwise
#' }
#'
#' @param dt A `data.table`.
#' @param varfrom A character vector containing one or more source column names.
#' @param varto A character string specifying the name of the output column.
#' @param val A value used to identify cases. Defaults to `1`.
#'
#' @return
#' A copy of `dt` with an additional binary column specified by `varto`.
#'
#' @examples
#' library(data.table)
#'
#' dt <- data.table(
#'   var1 = c(1, 0, NA, 0),
#'   var2 = c(0, 1, 1, NA)
#' )
#'
#' # Single source variable
#' is_case(dt, "var1", "case1")
#'
#' # Multiple source variables
#' is_case(dt, c("var1", "var2"), "case2")
#'
#' @export
is_case <- function(dt, varfrom, varto, val = 1) {

  # Work on a copy to avoid modifying the original data.table
  d <- data.table::copy(dt)

  if (length(varfrom) == 1) {

    # Single-column case
    d[, (varto) := data.table::fcase(
      is.na(get(varfrom)), 0L,
      get(varfrom) == val, 1L,
      default = 0L
    )]

  } else {

    # Multi-column case:
    # Return 1 if any selected column equals the target value
    d[, (varto) := as.integer(
      rowSums(.SD == val, na.rm = TRUE) > 0
    ),
    .SDcols = varfrom]

  }

  d
}
