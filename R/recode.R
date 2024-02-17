#' Overwrite values
#'
#' @description
#' [overwrite()] recodes values in `x` to a new set of values provided in `to`;
#' the values in `to` are recycled to match the length of `x`. By default,
#' missing values remain `NA`.
#'
#' @param x An atomic vector.
#' @param .na New value for missing values in `x`. Defaults to `NA`.
#'
#' @returns A vector of the same length of `x` with new values matching those
#'   in `to`.
#'
#' @examples
#' x <- c(letters[1:4], NA, NA)
#' # Recode all values to `"x"` but keep `NA`.
#' sdtm.oak:::overwrite(x, to = "x")
#'
#' # Recode all values to `"x"` but recode `NA` to a new value.
#' sdtm.oak:::overwrite(x, to = "x", .na = "x")
#' sdtm.oak:::overwrite(x, to = "x", .na = "Absent")
#'
#' # If `to` is not a scalar, it is recycled and matched by position for
#' # replacement.
#' sdtm.oak:::overwrite(x, to = c("x", "y"))
#'
#' # `x` can be of other types besides `character`, e.g. replace integers to a
#' # hard-coded new integer value.
#' sdtm.oak:::overwrite(x = 1:5, to = 0)
#'
#' # Example involving `logical` vectors
#' sdtm.oak:::overwrite(x = c(TRUE, FALSE), to = FALSE)
#'
#' # Returned type will be a type compatible with both the types of `to` and
#' # `.na`.
#' sdtm.oak:::overwrite(x = c("sdtm", "adam"), to = 0)
#' sdtm.oak:::overwrite(
#'   x = c("sdtm", "adam"),
#'   to = 0,
#'   .na = NA_character_
#' )
#' sdtm.oak:::overwrite(
#'   x = c("sdtm", "adam"),
#'   to = TRUE,
#'   .na = NA_real_
#' )
#'
#' @keywords internal
overwrite <- function(x, to, .na = NA) {
  # y <- rep_len(to, length(x))
  y <- rlang::rep_along(x, to)
  y[is.na(x)] <- .na

  y
}

#' Determine Indices for Rewriting
#'
#' [index_for_rewrite()] identifies the positions of elements in `x` that match
#' any of the values specified in the `from` vector. This function is primarily
#' used to facilitate the rewriting of values by pinpointing which elements in
#' `x` correspond to the `from` values and thus need to be replaced or updated.
#'
#' @param x A vector of values in which to search for matches.
#' @param from A vector of values to match against the elements in `x`.
#' @return An integer vector of the same length as `x`, containing the indices
#'   of the matched values from the `from` vector. If an element in `x` does not
#'   match any value in `from`, the corresponding position in the output will be
#'   `NA`. This index information is critical for subsequent rewrite operations.
#' @examples
#' sdtm.oak:::index_for_rewrite(x = 1:5, from = c(2, 4))
#'
#' @keywords internal
index_for_rewrite <- function(x, from) {
  match(x, from)
}

#' Are values to be rewritten?
#'
#' `are_to_rewrite` is a helper function designed to determine if any values
#' in a vector `x` match the specified `from` values, indicating they are
#' candidates for recoding or rewriting.
#'
#' @param x A vector of values that will be checked against the `from` vector.
#' @param from A vector of values that `x` will be checked for matches against.
#' @return A logical vector of the same length as `x`, where `TRUE` indicates
#'         that the corresponding value in `x` matches a value in `from` and
#'         should be rewritten, and `FALSE` otherwise. If `x` is empty, returns
#'         an empty logical vector. This function is intended for internal use
#'         and optimization in data transformation processes.
#' @keywords internal
#' @examples
#' sdtm.oak:::are_to_rewrite(x = 1:5, from = c(2, 4))
#'
#' sdtm.oak:::are_to_rewrite(letters[1:3], from = c("a", "c"))
#'
#' @keywords internal
are_to_rewrite <- function(x, from) {
  # match(x, from, nomatch = 0) != 0
  !is.na(index_for_rewrite(x, from))
}

#' Rewrite values
#'
#' [rewrite()] recodes values in `x` by matching elements in `from` onto values
#' in `to`.
#'
#' @param x An atomic vector of values are to be recoded.
#' @param from A vector of values to be matched in `x` for rewriting.
#' @param to A vector of values to be used as replacement for values in `from`.
#' @param .no_match Value to be used as replacement when cases in `from` are not
#'   matched.
#' @param .na Value to be used to recode missing values.
#'
#' @returns A vector of recoded values.
#'
#' @examples
#' x <- c("male", "female", "x", NA)
#' sdtm.oak:::rewrite(x,
#'   from = c("male", "female"),
#'   to = c("M", "F")
#' )
#' sdtm.oak:::rewrite(
#'   x,
#'   from = c("male", "female"),
#'   to = c("M", "F"),
#'   .no_match = "?"
#' )
#' sdtm.oak:::rewrite(
#'   x,
#'   from = c("male", "female"),
#'   to = c("M", "F"),
#'   .na = "missing"
#' )
#'
#' @keywords internal
rewrite <- function(x,
                    from,
                    to,
                    .no_match = x,
                    .na = NA) {
  to <- rlang::rep_along(x, to)
  index <- index_for_rewrite(x, from)
  y <- ifelse(!is.na(index), to[index], .no_match)
  y[is.na(x)] <- .na

  y
}
