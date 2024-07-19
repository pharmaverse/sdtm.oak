#' Determine Indices for Recoding
#'
#' [index_for_recode()] identifies the positions of elements in `x` that match
#' any of the values specified in the `from` vector. This function is primarily
#' used to facilitate the recoding of values by pinpointing which elements in
#' `x` correspond to the `from` values and thus need to be replaced or updated.
#'
#' @param x A vector of values in which to search for matches.
#' @param from A vector of values to match against the elements in `x`.
#' @return An integer vector of the same length as `x`, containing the indices
#'   of the matched values from the `from` vector. If an element in `x` does not
#'   match any value in `from`, the corresponding position in the output will be
#'   `NA`. This index information is critical for subsequent recoding operations.
#'
#' @keywords internal
index_for_recode <- function(x, from) {
  match(x, from)
}

#' Recode values
#'
#' [recode()] recodes values in `x` by matching elements in `from` onto values
#' in `to`.
#'
#' @param x An atomic vector of values are to be recoded.
#' @param from A vector of values to be matched in `x` for recoding.
#' @param to A vector of values to be used as replacement for values in `from`.
#' @param .no_match Value to be used as replacement when cases in `from` are not
#'   matched.
#' @param .na Value to be used to recode missing values.
#'
#' @returns A vector of recoded values.
#'
#' @keywords internal
recode <- function(
    x,
    from = unique(na.omit(x)),
    to = from,
    .no_match = x,
    .na = NA) {
  to <- vctrs::vec_recycle(to, length(from))
  index <- index_for_recode(x, from)
  y <- ifelse(!is.na(index), to[index], .no_match)
  y[is.na(x)] <- .na

  y
}
