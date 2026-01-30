#' Create Cannabis Population Indicators
#'
#' @description
#' Creates binary population indicators based on lifetime, last year, and/or
#' last month substance use variables. The function generates columns indicating
#' whether individuals have ever used, used in the past year, or used in the
#' past month. Users can specify which indicators to create.
#'
#' @param dt A data.table or data.frame containing the substance use variables.
#' @param types A character vector specifying which indicators to create.
#'   Must contain one or more of: "ltp" (lifetime), "lyp" (last year), "lmp" (last month).
#'   Default is \code{c("ltp", "lyp", "lmp")} to create all three indicators.
#'   The order and content of \code{types} must match the order and content of \code{vars}.
#' @param vars A character vector containing variable names corresponding to \code{types}.
#'   \itemize{
#'     \item If types = "ltp", provide 1 variable (lifetime use)
#'     \item If types = c("ltp", "lyp"), provide 2 variables (lifetime, last year)
#'     \item If types = c("ltp", "lyp", "lmp"), provide 3 variables (lifetime, last year, last month)
#'   }
#'   Variables should be coded where 1-2 indicate use.
#' @param value A character string to use as a suffix for the created variables.
#'   Default is "cannabis".
#'
#' @return A data.table with additional columns based on \code{types}:
#'   \describe{
#'     \item{ltpPop_\{value\}}{Lifetime population indicator (1 = ever used, 0 = never)}
#'     \item{lypPop_\{value\}}{Last year population indicator (1 = used in past year, 0 = no)}
#'     \item{lmpPop_\{value\}}{Last month population indicator (1 = used in past month, 0 = no)}
#'   }
#'
#' @details
#' The function creates cascading indicators:
#' \itemize{
#'   \item \strong{Lifetime (ltp):} 1 if lifetime use variable is 1 or 2
#'   \item \strong{Last year (lyp):} 1 if last year variable is 1-2, OR if lifetime variable is 2
#'   \item \strong{Last month (lmp):} 1 if last month variable is 1-2, OR if last year variable is 2,
#'         OR if lifetime variable is 2
#' }
#'
#' The cascading logic means:
#' \itemize{
#'   \item To create "lyp", you must also include "ltp" in types
#'   \item To create "lmp", you must also include both "ltp" and "lyp" in types
#' }
#'
#' @note
#' The input data.table is copied to avoid modifying by reference. Missing values
#' are handled as FALSE (coded as 0) in all conditions.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' # Create sample data
#' dt <- data.table(
#'   id = 1:5,
#'   lifetime_use = c(1, 2, 3, 1, NA),
#'   year_use = c(1, 2, 3, 2, 1),
#'   month_use = c(1, 3, 3, 1, 2)
#' )
#'
#' # Create all three indicators (default)
#' result <- create_cann_pop(
#'   dt = dt,
#'   vars = c("lifetime_use", "year_use", "month_use"),
#'   value = "cannabis"
#' )
#'
#' # Create only lifetime indicator
#' result <- create_cann_pop(
#'   dt = dt,
#'   types = "ltp",
#'   vars = "lifetime_use",
#'   value = "cannabis"
#' )
#'
#' # Create lifetime and last year indicators
#' result <- create_cann_pop(
#'   dt = dt,
#'   types = c("ltp", "lyp"),
#'   vars = c("lifetime_use", "year_use"),
#'   value = "cannabis"
#' )
#'
#' # With different substance
#' result <- create_cann_pop(
#'   dt = dt,
#'   types = c("ltp", "lyp", "lmp"),
#'   vars = c("lifetime_use", "year_use", "month_use"),
#'   value = "alcohol"
#' )
#' }
#'
#' @importFrom data.table copy fifelse fcase :=
#' @importFrom stats setNames
#' @export
create_cann_pop <- function(dt, types = c("ltp", "lyp", "lmp"), vars, value = "cannabis") {

  # Input validation
  if (!inherits(dt, c("data.table", "data.frame"))) {
    stop("dt must be a data.table or data.frame")
  }

  if (!is.character(types) || length(types) == 0) {
    stop("types must be a non-empty character vector")
  }

  # Validate types contains only valid values
  valid_types <- c("ltp", "lyp", "lmp")
  if (!all(types %in% valid_types)) {
    invalid_types <- types[!types %in% valid_types]
    stop(sprintf("Invalid types: %s. Must be one or more of: 'ltp', 'lyp', 'lmp'",
                 paste(invalid_types, collapse = ", ")))
  }

  # Check for duplicates in types
  if (any(duplicated(types))) {
    stop("types cannot contain duplicate values")
  }

  # Validate types and vars have same length
  if (length(types) != length(vars)) {
    stop(sprintf("Length of types (%d) must match length of vars (%d)",
                 length(types), length(vars)))
  }

  # Validate cascading requirements
  if ("lyp" %in% types && !"ltp" %in% types) {
    stop("To create 'lyp' indicator, 'ltp' must also be in types")
  }

  if ("lmp" %in% types && (!all(c("ltp", "lyp") %in% types))) {
    stop("To create 'lmp' indicator, both 'ltp' and 'lyp' must be in types")
  }

  # Validate vars order matches types order for cascading logic
  if (length(types) > 1) {
    # Check that types are in logical order
    type_positions <- match(types, valid_types)
    if (is.unsorted(type_positions)) {
      stop("types must be in order: 'ltp', 'lyp', 'lmp' (cascading order)")
    }
  }

  if (!all(vars %in% names(dt))) {
    missing_vars <- vars[!vars %in% names(dt)]
    stop(sprintf("Variables not found in dt: %s", paste(missing_vars, collapse = ", ")))
  }

  if (!is.character(value) || length(value) != 1 || nchar(value) == 0) {
    stop("value must be a non-empty character string of length 1")
  }

  # Create a copy to avoid modifying by reference
  dta <- data.table::copy(dt)

  # Ensure it's a data.table for := operator
  if (!data.table::is.data.table(dta)) {
    dta <- data.table::as.data.table(dta)
  }

  # Create named list mapping types to vars for clarity
  type_var_map <- setNames(vars, types)

  # Initialize variables to NULL for proper scoping
  var1 <- var2 <- var3 <- NULL

  # Create lifetime population indicator
  if ("ltp" %in% types) {
    var1 <- type_var_map["ltp"]
    val1 <- paste0("ltpPop_", value)
    dta[, (val1) := fifelse(get(var1) %in% 1:2, 1L, 0L)]
  }

  # Create last year population indicator
  if ("lyp" %in% types) {
    var2 <- type_var_map["lyp"]
    val2 <- paste0("lypPop_", value)
    dta[, (val2) := fcase(
      get(var2) %in% 1:2, 1L,
      get(var1) == 2, 1L,
      default = 0L
    )]
  }

  # Create last month population indicator
  if ("lmp" %in% types) {
    var3 <- type_var_map["lmp"]
    val3 <- paste0("lmpPop_", value)
    dta[, (val3) := fcase(
      get(var3) %in% 1:2, 1L,
      get(var2) == 2, 1L,
      get(var1) == 2, 1L,
      default = 0L
    )]
  }

  return(dta)
}
