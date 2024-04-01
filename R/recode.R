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
#' @examples
#' sdtm.oak:::index_for_recode(x = 1:5, from = c(2, 4))
#'
#' @keywords internal
index_for_recode <- function(x, from) {
  match(x, from)
}

#' Are values to be recoded?
#'
#' `are_to_recode` is a helper function designed to determine if any values
#' in a vector `x` match the specified `from` values, indicating they are
#' candidates for recoding.
#'
#' @param x A vector of values that will be checked against the `from` vector.
#' @param from A vector of values that `x` will be checked for matches against.
#' @return A logical vector of the same length as `x`, where `TRUE` indicates
#'         that the corresponding value in `x` matches a value in `from` and
#'         should be recoded, and `FALSE` otherwise. If `x` is empty, returns
#'         an empty logical vector. This function is intended for internal use
#'         and optimization in data transformation processes.
#' @keywords internal
#' @examples
#' sdtm.oak:::are_to_recode(x = 1:5, from = c(2, 4))
#'
#' sdtm.oak:::are_to_recode(letters[1:3], from = c("a", "c"))
#'
#' @keywords internal
are_to_recode <- function(x, from) {
  !is.na(index_for_recode(x, from))
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
#' @examples
#' x <- c("male", "female", "x", NA)
#' sdtm.oak:::recode(x,
#'   from = c("male", "female"),
#'   to = c("M", "F")
#' )
#' sdtm.oak:::recode(
#'   x,
#'   from = c("male", "female"),
#'   to = c("M", "F"),
#'   .no_match = "?"
#' )
#' sdtm.oak:::recode(
#'   x,
#'   from = c("male", "female"),
#'   to = c("M", "F"),
#'   .na = "missing"
#' )
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
