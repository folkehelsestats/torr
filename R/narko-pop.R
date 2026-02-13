#' Create Population Indicators for LTP and LYP Variables
#'
#' This function generates binary population indicators based on LTP (Lifetime prevalence)
#' and/or LYP (Last Year Prevalence) variables. It creates new columns in the dataset with
#' values of 1 (in population) or 0 (not in population) based on specified conditions.
#'
#' @param d A data.table or data.frame. The input dataset.
#' @param types A character vector specifying which population types to calculate.
#'   Options are "ltp" and/or "lyp". Default is \code{c("ltp", "lyp")}.
#' @param vars A named or unnamed character vector of variable names corresponding to the types.
#'   If named, names should match the types. If unnamed, order should match the order in \code{types}.
#' @param val A character string used as a suffix for the created variable names.
#'   New variables will be named \code{ltpPop_<val>} and/or \code{lypPop_<val>}.
#' @param ans A character string specifying the name of the answer/response column
#'   in the dataset for ltp drug question. Default is \code{"ans1"}.
#'
#' @return A data.table with new population indicator columns added. The original
#'   data.table is not modified (a copy is made internally).
#'
#' @details
#' The function creates population indicators based on the following logic:
#' \itemize{
#'   \item For LTP: Value is 1 if the LTP variable is 1 or 2, OR if LTP is missing
#'         and the answer variable ans1 is equals 2. Otherwise 0.
#'   \item For LYP: Value is 1 if the LYP variable is 1 or 2, OR if LYP is missing
#'         and LTP equals 2, OR if LYP is missing and the answer variable ans2 is equals 2.
#'         Otherwise 0.
#' }
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' # Create sample data
#' dt <- data.table(
#'   id = 1:5,
#'   ltp_var = c(1, 2, NA, 3, NA),
#'   lyp_var = c(1, NA, 2, NA, NA),
#'   ans1 = c(1, 1, 2, 1, 2)
#' )
#'
#' # Calculate both LTP and LYP populations
#' result <- narko_pop(
#'   d = dt,
#'   types = c("ltp", "lyp"),
#'   vars = c("ltp_var", "lyp_var"),
#'   val = "2024"
#' )
#'
#' # Calculate only LTP population
#' result_ltp <- narko_pop(
#'   d = dt,
#'   types = "ltp",
#'   vars = "ltp_var",
#'   val = "2024"
#' )
#' }
#'
#' @importFrom data.table copy fcase
#' @export
create_narko_pop <- function(d, types = c("ltp", "lyp"), vars, val, ans = "ans1") {
  # Input validation
  if (!inherits(d, c("data.table", "data.frame"))) {
    stop("'d' must be a data.table or data.frame")
  }

  if (!all(types %in% c("ltp", "lyp"))) {
    stop("'types' must contain only 'ltp' and/or 'lyp'")
  }

  if (length(vars) != length(types)) {
    stop("Length of 'vars' must equal length of 'types'")
  }

  if (!ans %in% names(d)) {
    stop(sprintf("Column '%s' not found in dataset", ans))
  }

  # Ensure it's a data.table for := operator
  if (!data.table::is.data.table(d)) {
    d <- data.table::as.data.table(d)
  }

  # Make a copy to avoid modifying the original
  d <- data.table::copy(d)

  # Create named vector of variables
  vx <- setNames(vars, types)

  # Calculate LTP population if requested
  if ("ltp" %in% types) {
    var1 <- vx["ltp"]

    if (!var1 %in% names(d)) {
      stop(sprintf("Column '%s' not found in dataset", var1))
    }

    vn <- paste0("ltpPop_", val)
    d[, (vn) := data.table::fcase(
      get(var1) %in% 1:2, 1L,
      is.na(get(var1)) & get(ans) == 2, 1L,
      default = 0L
    )]
  }

  # Calculate LYP population if requested
  if ("lyp" %in% types) {
    var1 <- vx["ltp"]
    var2 <- vx["lyp"]

    if (!var2 %in% names(d)) {
      stop(sprintf("Column '%s' not found in dataset", var2))
    }

    vn2 <- paste0("lypPop_", val)
    d[, (vn2) := data.table::fcase(
      get(var2) %in% 1:2, 1L,
      is.na(get(var2)) & get(var1) == 2, 1L,
      is.na(get(var2)) & get(ans) == 2, 1L,
      default = 0L
    )]
  }

  return(d)
}



#' Apply `create_narko_pop()` Over Multiple Specifications
#'
#' @description
#' A convenience wrapper that applies [torr::create_narko_pop()] repeatedly to
#' the same dataset for a set of `(vars, val)` specifications. This helps avoid
#' writing multiple sequential calls and keeps analysis code concise and
#' consistent.
#'
#' @param D
#'   A data.frame or data.table to be augmented. The object is passed to and
#'   returned from each call to [torr::create_narko_pop()].
#' @param vars_list
#'   A list where each element is a character vector of variable names to pass
#'   as `vars` to [torr::create_narko_pop()]. For example:
#'   `list(c("ans2_a","ans3_1"), c("ans2_b","ans3_2"))`.
#' @param vals
#'   A character vector of the same length as `vars_list`. Each element is
#'   passed as `val` to [torr::create_narko_pop()] for the corresponding
#'   `vars_list` entry.
#' @param ...
#'   Additional arguments passed through to [torr::create_narko_pop()], if
#'   supported by that function (e.g., `types`, `ans`, etc.).
#'
#' @details
#' Internally, the function loops once over `seq_along(vals)`, updating `D`
#' in-place for each `(vars, val)` pair by calling [torr::create_narko_pop()].
#' The order of `vars_list` / `vals` determines the order of application.
#'
#' Basic validation is performed to ensure:
#' - `vars_list` is a list.
#' - `vals` is a character vector with the same length as `vars_list`.
#' - Each element of `vars_list` is a non-empty character vector.
#'
#' @return
#' Returns the updated object `D` after all calls to
#' [torr::create_narko_pop()] have been applied.
#'
#' @seealso
#' [torr::create_narko_pop()]
#'
#' @examples
#' \dontrun{
#' # Example specification (mirrors repeated calls):
#' vars_list <- list(
#'   c("ans2_a", "ans3_1"),
#'   c("ans2_b", "ans3_2"),
#'   c("ans2_c", "ans3_3"),
#'   c("ans2_e", "ans3_5"),
#'   c("ans2_f", "ans3_6"),
#'   c("ans2_g", "ans3_7"),
#'   c("ans2_h", "ans3_8")
#' )
#' vals <- c("kokain", "mdma", "amfetaminer", "heroin", "ghb", "lsd", "annet")
#'
#' DD2 <- torr::create_narko_pop_vec(DD, vars_list, vals)
#' }
#'
#' @export
create_narko_pop_vec <- function(D, vars_list, vals, ...) {
  # --- Validation -------------------------------------------------------------
  if (!is.list(vars_list)) {
    stop("`vars_list` must be a list of character vectors.", call. = FALSE)
  }
  if (!is.character(vals)) {
    stop("`vals` must be a character vector.", call. = FALSE)
  }
  if (length(vars_list) != length(vals)) {
    stop("`vars_list` and `vals` must have the same length.", call. = FALSE)
  }
  if (length(vars_list) == 0L) {
    # Nothing to do; return D unchanged
    return(D)
  }

  # Check each element of vars_list
  bad_idx <- vapply(
    vars_list,
    FUN = function(x) !(is.character(x) && length(x) >= 1L && all(nzchar(x))),
    FUN.VALUE = logical(1)
  )
  if (any(bad_idx)) {
    idx <- which(bad_idx)[1L]
    stop(
      sprintf(
        "`vars_list[[%d]]` must be a non-empty character vector with non-empty names.",
        idx
      ),
      call. = FALSE
    )
  }

  # --- Iteration --------------------------------------------------------------
  for (i in seq_along(vals)) {
    D <- torr::create_narko_pop(D, vars = vars_list[[i]], val = vals[i], ...)
  }
  D
}
