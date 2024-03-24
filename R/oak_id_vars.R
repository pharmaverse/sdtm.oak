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

#' Does a vector contain the raw dataset key variables?
#'
#' [contains_oak_id_vars()] evaluates whether a character vector `x` contains
#' the raw dataset key variable names, i.e. the so called Oak identifier
#' variables --- these are defined by the return value of [oak_id_vars()].
#'
#' @param x A character vector.
#'
#' @returns A logical scalar value.
#'
#' @examples
#' # `oak_id_vars()` is the function that defines what are the minimal set of
#' # oak keys. Hence, by definition, the following code should always return
#' # `TRUE`.
#' sdtm.oak:::contains_oak_id_vars(oak_id_vars())
#'
#' # Returns `FALSE`.
#' sdtm.oak:::contains_oak_id_vars(character())
#'
#' # Another example that returns `FALSE` because it is missing
#' # `"patient_number"`.
#' sdtm.oak:::contains_oak_id_vars(c("oak_id", "raw_source"))
#'
#' @keywords internal
contains_oak_id_vars <- function(x) {
  admiraldev::assert_character_vector(x)
  all(oak_id_vars() %in% x)
}
