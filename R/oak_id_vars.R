#' Raw dataset keys
#'
#' [oak_id_vars()] is a helper function providing the variable (column) names to
#' be regarded as keys in [tibbles][tibble::tibble-package] representing raw
#' datasets. By default, the set of names is
#' `r knitr::combine_words(oak_id_vars())`. Extra variable names may be
#' indicated and passed in `extra_vars` which are appended to the default names.
#'
#' @param extra_vars A character vector of extra column names to be appended to
#'   the default names: `r knitr::combine_words(oak_id_vars())`.
#'
#' @returns A character vector of column names to be regarded
#' as keys in raw datasets.
#'
#' @examples
#' sdtm.oak:::oak_id_vars()
#'
#' sdtm.oak:::oak_id_vars(extra_vars = "sample_id")
#'
#' @keywords internal
oak_id_vars <- function(extra_vars = NULL) {

  admiraldev::assert_character_vector(extra_vars, optional = TRUE)
  unique(c("oak_id", "raw_source", "patient_number", extra_vars))

}
