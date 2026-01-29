#' Create Population Indicator Columns for LTP and LYP Analysis
#'
#' @description
#' This function creates binary population indicator columns for LTP (Lifetime Prevalence)
#' and LYP (Last Year Prevalence) based on response patterns in drug use survey data.
#' The population is defined by specific answer combinations across multiple question variables.
#'
#' @param data A data.table containing the survey response data. Must include an 'ans1' column.
#' @param ltp Character vector of column names representing LTP question variables (e.g., ans2_x variables).
#' @param lyp Character vector of column names representing LYP question variables (e.g., ans3_x variables).
#' @param narkvars Character vector of drug/substance names corresponding to the question variables.
#'   Used for naming the output columns.
#'
#' @return A data.table (copy of input) with additional binary indicator columns:
#'   \itemize{
#'     \item \code{ltpPop_<substance>}: 1 if respondent is in LTP population for that substance, 0 otherwise
#'     \item \code{lypPop_<substance>}: 1 if respondent is in LYP population for that substance, 0 otherwise
#'   }
#'
#' @details
#' ## Population Definition Logic
#'
#' The population indicators are calculated based on the following rules:
#'
#' \subsection{Core Population (applies to both LTP and LYP):}{
#'   \itemize{
#'     \item Rule 1: All respondents who answered 1 or 2 ie. Yes and No, in Ans1 are included
#'   }
#' }
#'
#' \subsection{LTP Population (ltpPop_*):}{
#'   A respondent is coded as 1 (in population) if:
#'   \itemize{
#'     \item They answered 1 or 2 in the corresponding Ans2_x variable, OR
#'     \item The Ans2_x variable is missing (NA) AND Ans1 is 2
#'   }
#'   Otherwise coded as 0 (not in population)
#' }
#'
#' \subsection{LYP Population (lypPop_*):}{
#'   A respondent is coded as 1 (in population) if:
#'   \itemize{
#'     \item They answered 1 or 2 in the corresponding Ans3_x variable, OR
#'     \item The Ans3_x variable is missing (NA) AND Ans1 is 2
#'   }
#'   Otherwise coded as 0 (not in population)
#' }
#'
#' \subsection{Exclusion Rule:}{
#'   \itemize{
#'     \item Missing values in ans2_x and ans3_x are excluded (coded as 0) if ans1 is NOT in \{1, 2\}
#'   }
#' }
#'
#' @note
#' \itemize{
#'   \item The function creates a copy of the input data to avoid modifying the original
#'   \item Requires data.table package
#'   \item The length of \code{ltp}, \code{lyp}, and \code{narkvars} should match
#'   \item Column names are automatically generated as \code{ltpPop_<substance>} and \code{lypPop_<substance>}
#' }
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' # Sample data
#' dt <- data.table(
#'   ans1 = c(1, 2, 3, 1, 2),
#'   ans2_cannabis = c(1, 2, 8, NA, 1),
#'   ans2_cocaine = c(2, NA, 9, 1, NA),
#'   ans3_cannabis = c(1, 2, NA, 1, 2),
#'   ans3_cocaine = c(NA, 1, 2, NA, 1)
#' )
#'
#' # Create population indicators
#' result <- create_narko_pop(
#'   data = dt,
#'   ltp = c("ans2_cannabis", "ans2_cocaine"),
#'   lyp = c("ans3_cannabis", "ans3_cocaine"),
#'   narkvars = c("cannabis", "cocaine")
#' )
#'
#' # Result includes ltpPop_cannabis, ltpPop_cocaine, lypPop_cannabis, lypPop_cocaine
#' }
#'
#' @family population indicator functions
#' @importFrom data.table is.data.table copy fifelse setnames
#' @export
create_narko_pop <- function(data, ltp, lyp, narkvars) {

  ans1 <- NULL

  # Input validation
  if (!data.table::is.data.table(data)) {
    stop("'data' must be a data.table")
  }
  if (!"ans1" %in% names(data)) {
    stop("'data' must contain 'ans1' column")
  }
  if (length(ltp) != length(narkvars) || length(lyp) != length(narkvars)) {
    stop("'ltp', 'lyp', and 'narkvars' must have the same length")
  }

  # Check that all specified columns exist
  missing_ltp <- setdiff(ltp, names(data))
  if (length(missing_ltp) > 0) {
    stop(sprintf("LTP columns not found in data: %s", paste(missing_ltp, collapse = ", ")))
  }

  missing_lyp <- setdiff(lyp, names(data))
  if (length(missing_lyp) > 0) {
    stop(sprintf("LYP columns not found in data: %s", paste(missing_lyp, collapse = ", ")))
  }

  # Create a copy to avoid modifying original data
  dta <- data.table::copy(data)

  # Those being filtered ie. answered No to Ans1
  # Use fifelse to handle NA values properly (NA becomes FALSE)
  np <- dta[, data.table::fifelse(is.na(ans1), FALSE, ans1 == 2)]

  ## Vectorized ltp columns
  # Create binary indicators for LTP population
  # 1 if: answered 1 or 2, OR answered No in Ans1
  # 0 otherwise
  ltp_cols <- paste0("ltp_", ltp)
  dta[, (ltp_cols) := lapply(.SD, function(x) {
    data.table::fifelse(x %in% c(1, 2), 1,
            data.table::fifelse(is.na(x) & np, 1, 0))
  }), .SDcols = ltp]

  # Rename to final column names
  ltpVars <- paste0("ltpPop_", narkvars)
  data.table::setnames(dta, old = ltp_cols, new = ltpVars)

  ## Vectorized lyp columns
  # Create binary indicators for LYP population
  # 1 if: answered 1 or 2, OR answered No in Ans1
  # 0 otherwise
  lyp_cols <- paste0("lyp_", lyp)
  dta[, (lyp_cols) := lapply(.SD, function(x) {
    data.table::fifelse(x %in% c(1,2), 1,
            data.table::fifelse(is.na(x) & np, 1, 0))
  }), .SDcols = lyp]

  # Rename to final column names
  lypVars <- paste0("lypPop_", narkvars)
  data.table::setnames(dta, old = lyp_cols, new = lypVars)

  return(dta)
}
