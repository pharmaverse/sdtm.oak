#' Subject-level key variables
#'
#' [sbj_vars()] returns the set of variable names that uniquely define
#' a subject.
#'
#' @returns A character vector of variable names.
#'
#' @examples
#' sbj_vars()
#'
#' @export
sbj_vars <- function() {
  c("STUDYID", "USUBJID")
}
