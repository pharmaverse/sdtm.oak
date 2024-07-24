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
#' @export
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
#' @keywords internal
contains_oak_id_vars <- function(x) {
  admiraldev::assert_character_vector(x)
  all(oak_id_vars() %in% x)
}


#' A function to generate oak_id_vars
#'
#' @param raw_dat The raw dataset (dataframe)
#' @param pat_var Variable that holds the patient number
#' @param raw_src Name of the raw source
#'
#' @return dataframe
#' @export
#'
#' @examples
#' raw_dataset <-
#'   tibble::tribble(
#'     ~patnum, ~MDRAW,
#'     101L, "BABY ASPIRIN",
#'     102L, "CORTISPORIN",
#'     103L, NA_character_,
#'     104L, "DIPHENHYDRAMINE HCL"
#'   )
#'
#' # Generate oak_id_vars
#' generate_oak_id_vars(
#'   raw_dat = raw_dataset,
#'   pat_var = "patnum",
#'   raw_src = "Concomitant Medication"
#' )
generate_oak_id_vars <- function(raw_dat,
                                 pat_var,
                                 raw_src) {
  admiraldev::assert_character_scalar(pat_var)
  admiraldev::assert_character_scalar(raw_src)
  admiraldev::assert_data_frame(raw_dat)
  admiraldev::assert_data_frame(raw_dat, required_vars = rlang::syms(pat_var))

  raw_oak_id_vars <- raw_dat |>
    dplyr::mutate(
      oak_id = structure(seq_len(nrow(raw_dat))),
      patient_number = !!rlang::sym(pat_var),
      raw_source = raw_src
    ) |>
    dplyr::select(oak_id_vars(), dplyr::everything())

  return(raw_oak_id_vars)
}
