#' Record-level key variables
#'
#' [rec_vars()] returns the set of variable names that uniquely define
#' a record in a given SDTM domain.
#'
#' @param domain An SDTM domain in abbreviated form, e.g. `"DM"` (Demographics)
#'   or `"CM"` (Concomitant Medications).
#'
#' @returns A character vector of variable names.
#'
#' @examples
#' rec_vars("DM")
#'
#' rec_vars("AE")
#'
#' @export
rec_vars <- function(domain) {

  domain <- toupper(domain)
  domain <- rlang::arg_match(domain, values = names(domain_record_vars))
  domain_record_vars[[domain]]
}

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
