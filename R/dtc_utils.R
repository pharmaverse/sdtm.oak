#' Assert date time character formats
#'
#' [assert_dtc_fmt()] takes a character vector of date/time formats and checks if
#' the formats are supported, meaning it checks if they are one of the formats
#' listed in column `fmt` of [dtc_formats], failing with an error otherwise.
#'
#' @param fmt A character vector.
#'
#' @keywords internal
assert_dtc_fmt <- function(fmt) {
  rlang::arg_match(fmt,
    values = sdtm.oak::dtc_formats$fmt,
    multiple = TRUE
  )
}

#' Assert dtc format
#'
#' [assert_dtc_format()] is an internal helper function aiding with the checking
#' of the `.format` parameter of [create_iso8601()].
#'
#' @param .format The argument of [create_iso8601()]'s `.format` parameter.
#'
#' @returns This function throws an error if `.format` is not either:
#' - A character vector of formats permitted by [assert_dtc_fmt()];
#' - A list of character vectors of formats permitted by [assert_dtc_fmt()].
#'
#' Otherwise, it returns `.format` invisibly.
#'
#' @keywords internal
assert_dtc_format <- function(.format) {
  abort_msg <- "`.format` must be either a character vector of formats of a list thereof."

  switch(typeof(.format),
    character = assert_dtc_fmt(.format),
    list = purrr::map(.format, assert_dtc_format),
    cli::cli_abort(abort_msg)
  )

  invisible(.format)
}

#' Assert capture matrix
#'
#' @description
#'
#' [assert_capture_matrix()] is an internal helper function aiding with the
#' checking of an internal R object that contains the parsing results as
#' returned by [parse_dttm()]: capture matrix.
#'
#' This function checks that the capture matrix is a matrix and that it contains
#' six columns: `year`, `mon`, `mday`, `hour`, `min` and `sec`.
#'
#' @param m A character matrix.
#'
#' @returns This function throws an error if `m` is not either:
#' - A character matrix;
#' - A matrix whose columns are (at least): `year`, `mon`, `mday`, `hour`,
#'   `min` and `sec`.
#'
#' Otherwise, it returns `m` invisibly.
#'
#' @keywords internal
assert_capture_matrix <- function(m) {
  # `m` must be of character type.
  admiraldev::assert_character_vector(m)

  if (!is.matrix(m)) {
    cli::cli_abort("`m` must be a matrix.")
  }

  col_names <- c("year", "mon", "mday", "hour", "min", "sec")
  m_col_names <- colnames(m)
  if (is.null(m_col_names) || !all(m_col_names == col_names)) {
    cli::cli_abort("`m` must have the following colnames: `year`, `mon`, `mday`, `hour`, `min` and `sec`.")
  }

  invisible(m)
}

#' Complete a capture matrix
#'
#' [complete_capture_matrix()] completes the missing, if any, columns of the
#' capture matrix.
#'
#' @param m A character matrix that might be missing one or more of the
#' following columns: `year`, `mon`, `mday`, `hour`, `min` or `sec`.
#'
#' @returns A character matrix that contains the columns `year`, `mon`, `mday`,
#'   `hour`, `min` and `sec`. Any other existing columns are dropped.
#'
#' @keywords internal
complete_capture_matrix <-
  function(m) {
    col_names <- c("year", "mon", "mday", "hour", "min", "sec")

    # If all columns are already present, and in the correct order,
    # then simply return.
    if (identical(col_names, colnames(m))) {
      return(m)
    }

    # If all columns are present but not in the right order, then reorder.
    if (setequal(col_names, colnames(m))) {
      return(m[, col_names, drop = FALSE])
    }

    miss_cols <- setdiff(col_names, colnames(m))
    miss_n_cols <- length(miss_cols)

    m2 <- matrix(nrow = nrow(m), ncol = miss_n_cols)
    colnames(m2) <- miss_cols

    m3 <- cbind(m, m2)[, col_names, drop = FALSE]
    assert_capture_matrix(m3)
  }

#' Coalesce capture matrices
#'
#' [coalesce_capture_matrices()] combines several capture matrices into one.
#' Each argument of `...` should be a capture matrix in the sense of the output
#' by [complete_capture_matrix()], meaning a character matrix of six columns
#' whose names are: `year`, `mon`, `mday`, `hour`, `min` or `sec`.
#'
#' @param ... A sequence of capture matrices.
#'
#' @returns A single capture matrix whose values have been coalesced in the
#' sense of [coalesce()][dplyr::coalesce].
#'
#' @keywords internal
coalesce_capture_matrices <- function(...) {
  dots <- rlang::list2(...)

  if (rlang::is_empty(dots)) {
    cli::cli_abort("At least one input must be passed.")
  }

  # Assert that every argument in `...` is a capture matrix
  purrr::walk(dots, assert_capture_matrix)

  # `as.vector` needed because of: https://github.com/tidyverse/dplyr/issues/6954
  vecs <- purrr::map(dots, as.vector)
  vec <- dplyr::coalesce(!!!vecs)
  m <- matrix(vec, ncol = 6L)
  colnames(m) <- c("year", "mon", "mday", "hour", "min", "sec")

  m
}
