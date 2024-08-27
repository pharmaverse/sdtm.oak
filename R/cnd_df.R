# ------------------------------------------------------------------------------
# File: cnd_df.R
# Package: sdtm.oak
# Author: Ramiro Magno <rmagno@pattern.institute>
# Created: 2024-05-23
# Last Modified: 2024-05-15
# ------------------------------------------------------------------------------
# Description:
#
# This file contains functions and scripts related to the so-called conditioned
# data frames, i.e. those data frames extended with class `cnd_df`.
#
# Functions:
#
# - `new_cnd_df()`: Create a "conditioned data set" (class cnd_df).
# - `is_cnd_df()`: Assess whether the argument is a `cnd_df` data frame.
# - `get_cnd_df_cnd()`: Extract the attribute "cnd" from a `cnd_df` data frame.
# - `get_cnd_df_cnd_sum()`: Extract the attribute "cnd_sum" from a `cnd_df` data frame.
# - `rm_cnd_df()`: De-class a cnd_df data frame.
# - `tbl_sum.cnd_df()`: Print method for `cnd_df` tibbles.
# - `ctl_new_rowid_pillar.cnd_df()`: Print method for the row ids cnd_df` tibbles.
# - `eval_conditions()`: Find which rows match a set of conditions.
# - `condition_add()`: Create a conditioned data frame (user facing).
# - `mutate.cnd_df()`: Mutate a conditioned data frame.

#' Create a data frame with filtering tags
#'
#' [new_cnd_df()] creates a _conditioned_ data frame, classed `cnd_df`, meaning
#' that this function extends the data frame passed as argument by storing a
#' logical vector `cnd` (as attribute) that marks rows for posterior conditional
#' transformation by methods that support _conditioned_ data frames.
#'
#' @param dat A data frame.
#' @param cnd A logical vector. Length must match the number of rows in `dat`.
#' @param .warn Whether to warn about creating a new _conditioned_ data frame
#' in case that `dat` already is one.
#'
#' @returns A data frame `dat` with the additional class `"cnd_df"` and the
#'   following attributes:
#'
#' - `cnd`: The logical vector passed as argument `cnd`: `TRUE` values mark
#' rows in `dat` to be used for transformations; rows marked with `FALSE` are
#' not transformed; and `NA` mark rows whose transformations are to be applied
#' resulting in `NA`.
#' - `cnd_sum`: An integer vector of three elements providing the sum of `TRUE`,
#' `FALSE` and `NA` values in `cnd`, respectively.
#'
#' @seealso [is_cnd_df()], [get_cnd_df_cnd()], [get_cnd_df_cnd_sum()],
#'   [rm_cnd_df()].
#'
#' @keywords internal
new_cnd_df <- function(dat, cnd, .warn = TRUE) {
  admiraldev::assert_data_frame(dat)
  assert_logical_vector(cnd)

  if (!identical(nrow(dat), length(cnd))) {
    msg <- c(
      "Number of rows in `dat` must match length of `cnd`."
    )
    cli::cli_abort(message = msg)
  }

  is_cnd_df <- inherits(dat, "cnd_df")
  if (.warn && is_cnd_df) {
    msg <- "`dat` is already a conditioned data frame (`cnd_df`)."
    cli::cli_warn(message = msg)
  }

  if (!is_cnd_df) {
    dat <- tibble::as_tibble(dat)
    class(dat) <- c("cnd_df", class(dat))
  }

  n_true <- sum(cnd, na.rm = TRUE)
  n_false <- sum(!cnd, na.rm = TRUE)
  n_na <- length(cnd) - (n_true + n_false)
  cnd_sum <- c(n_true = n_true, n_false = n_false, n_na = n_na)

  attr(dat, "cnd") <- cnd
  attr(dat, "cnd_sum") <- cnd_sum

  return(dat)
}

#' Check if a data frame is a conditioned data frame
#'
#' [is_cnd_df()] checks whether a data frame is a conditioned data frame, i.e.
#' of class `cnd_df`.
#'
#' @param dat A data frame.
#' @return `TRUE` if `dat` is a conditioned data frame (class `cnd_df`),
#'   otherwise `FALSE`.
#'
#' @seealso [new_cnd_df()], [get_cnd_df_cnd()], [get_cnd_df_cnd_sum()],
#'   [rm_cnd_df()].
#'
#' @keywords internal
is_cnd_df <- function(dat) {
  inherits(dat, "cnd_df")
}

#' Get the conditioning vector from a conditioned data frame
#'
#' [get_cnd_df_cnd()] extracts the conditioning vector from a conditioned data
#' frame, i.e. from an object of class `cnd_df`.
#'
#' @param dat A conditioned data frame (`cnd_df`).
#' @return The conditioning vector (`cnd`) if `dat` is a conditioned data frame
#'   (`cnd_df`), otherwise `NULL`, or `NULL` if `dat` is not a conditioned data
#'   frame (`cnd_df`).
#'
#' @seealso [new_cnd_df()], [is_cnd_df()], [get_cnd_df_cnd_sum()],
#'   [rm_cnd_df()].
#'
#' @keywords internal
get_cnd_df_cnd <- function(dat) {
  if (is_cnd_df(dat)) {
    attr(dat, "cnd")
  } else {
    NULL
  }
}

#' Get the summary of the conditioning vector from a conditioned data frame
#'
#' [get_cnd_df_cnd_sum()] extracts the tally of the conditioning vector from a
#' conditioned data frame.
#'
#' @param dat A conditioned data frame (`cnd_df`).
#' @return A vector of three elements providing the sum of `TRUE`, `FALSE`, and
#'   `NA` values in the conditioning vector (`cnd`), or `NULL` if `dat` is not
#'   a conditioned data frame (`cnd_df`).
#'
#' @seealso [new_cnd_df()], [is_cnd_df()], [get_cnd_df_cnd()], [rm_cnd_df()].
#'
#' @keywords internal
get_cnd_df_cnd_sum <- function(dat) {
  if (is_cnd_df(dat)) {
    attr(dat, "cnd_sum")
  } else {
    NULL
  }
}

#' Remove the `cnd_df` class from a data frame
#'
#' This function removes the `cnd_df` class, along with its attributes, if
#' applicable.
#'
#' @param dat A data frame.
#' @return The input `dat` without the `cnd_df` class and associated attributes.
#'
#' @seealso [new_cnd_df()], [is_cnd_df()], [get_cnd_df_cnd()],
#'   [get_cnd_df_cnd_sum()].
#'
#' @keywords internal
rm_cnd_df <- function(dat) {
  if (is_cnd_df(dat)) {
    class(dat) <- class(dat)[class(dat) != "cnd_df"]
    attr(dat, "cnd") <- NULL
    attr(dat, "cnd_sum") <- NULL
  }
  return(dat)
}

#' Conditioned tibble header print method
#'
#' Conditioned tibble header print method. This S3 method adds an extra line
#' in the header of a tibble that indicates the tibble is a conditioned tibble
#' (`# Cond. tbl:`) followed by the tally of the conditioning vector: number
#' of TRUE, FALSE and NA values: e.g., `1/1/1`.
#'
#' @param x A conditioned tibble of class `cnd_df`.
#' @param ... Additional arguments passed to the default print method.
#'
#' @importFrom pillar tbl_sum
#'
#' @seealso [ctl_new_rowid_pillar.cnd_df()].
#'
#' @return A character vector with header values of the conditioned data frame.
#'
#' @examples
#' df <- data.frame(x = c(1L, NA_integer_, 3L))
#' (cnd_df <- condition_add(dat = df, x >= 2L))
#' pillar::tbl_sum(cnd_df)
#'
#' @export
tbl_sum.cnd_df <- function(x, ...) {
  default_header <- NextMethod()

  tally <- get_cnd_df_cnd_sum(x)
  h2 <- sprintf("%d/%d/%d", tally[1L], tally[2L], tally[3L])
  c(default_header, "Cond. tbl" = h2)
}

lgl_to_chr <- function(x) {
  dplyr::case_match(x, TRUE ~ "T", FALSE ~ "F", NA ~ "-")
}

#' Conditioned tibble pillar print method
#'
#' @inheritParams pillar::ctl_new_rowid_pillar
#' @importFrom pillar ctl_new_rowid_pillar
#'
#' @return A character vector to print the tibble which is a conditioned dataframe.
#'
#' @seealso [tbl_sum.cnd_df()].
#'
#' @export
ctl_new_rowid_pillar.cnd_df <- function(controller, x, width, ...) {
  out <- NextMethod()
  n_row <- nrow(x)
  idx <- seq_len(n_row)
  i <- sprintf("%d", idx)
  i_width <- nchar(as.character(i))
  i_max_width <- max(i_width)
  max_width <- i_max_width + 2L
  ws <- strrep(" ", max_width - i_width - 1L)
  abb_lgl <- lgl_to_chr(attr(controller, "cnd")[idx])

  row_ids <- paste0(i, ws, abb_lgl)
  width <- max(nchar(as.character(row_ids)))
  pillar::new_pillar(
    list(
      title = out$title,
      type = out$type,
      data = pillar::pillar_component(
        pillar::new_pillar_shaft(list(row_ids = row_ids),
          width = width,
          class = "pillar_rif_shaft"
        )
      )
    ),
    width = width
  )
}

#' Evaluate conditions
#'
#' @description
#' [eval_conditions()] evaluates a set of conditions in the context of a
#' data frame and an optional environment.
#'
#' The utility of this function is to provide an easy way to generate a logical
#' vector of matching records from a set of logical conditions involving
#' variables in a data frame (`dat`) and optionally in a supplementary
#' environment (`.env`). The set of logical conditions are provided as
#' expressions to be evaluated in the context of `dat` and `.env`.
#'
#' Variables are looked up in `dat`, then in `.env`, then in the calling
#' function's environment, followed by its parent environments.
#'
#' @param dat A data frame
#' @param ... A set of logical conditions, e.g. `y & z, x | z` (`x`, `y`, `z`
#'   would have to exist either as columns in `dat` or in the environment
#'   `.env`). If multiple expressions are included, they are combined with the
#'   `&` operator.
#' @param .na Return value to be used when the conditions evaluate to `NA`.
#' @param .env An optional environment to look for variables involved in logical
#'   expression passed in `...`. A data frame or a list can also be passed that
#'   will be coerced to an environment internally.
#'
#' @returns A logical vector reflecting matching rows in `dat`.
#'
#' @keywords internal
eval_conditions <- function(dat,
                            ...,
                            .na = NA,
                            .env = rlang::caller_env()) {
  conditions <- rlang::enexprs(...)

  # List (or data frame).
  if (is.list(.env)) {
    .env <- rlang::as_environment(.env, parent = rlang::caller_env())
  }

  lgl_vctrs <-
    conditions |>
    purrr::map(~ rlang::eval_tidy(.x, dat, env = .env)) |>
    purrr::map(~ dplyr::if_else(is.na(.x), .na, .x))

  cnd <- purrr::reduce(lgl_vctrs, `&`, .init = rep(TRUE, nrow(dat)))

  cnd
}

#' Add filtering tags to a data set
#'
#' @description
#' `condition_add()` tags records in a data set, indicating which rows match the
#' specified conditions, resulting in a conditioned data frame. Learn how to
#' integrate conditioned data frames in your SDTM domain derivation in
#' `vignette("cnd_df")`.
#'
#' @param dat A data frame.
#' @param ... Conditions to filter the data frame.
#' @param .na Return value to be used when the conditions evaluate to `NA`.
#' @param .dat2 An optional environment to look for variables involved in
#'   logical expression passed in `...`. A data frame or a list can also be
#'   passed that will be coerced to an environment internally.
#'
#' @returns A conditioned data frame, meaning a tibble with an additional class
#'   `cnd_df` and a logical vector attribute indicating matching rows.
#'
#' @examples
#' (df <- tibble::tibble(x = 1L:3L, y = letters[x]))
#'
#' # Mark rows for which `x` greater than `1`
#' (cnd_df <- condition_add(dat = df, x > 1L))
#'
#' @export
condition_add <- function(dat, ..., .na = NA, .dat2 = rlang::env()) {
  admiraldev::assert_data_frame(dat)
  # TODO: assertion for `...` (perhaps with admiraldev::assert_filter_cond()?)
  # TODO: assertion for `.na`
  # TODO: assertion for `.dat2`

  if (is_cnd_df(dat)) {
    cli::cli_warn(
      c(
        "`dat` is already a conditioned data frame (`cnd_df`).",
        "The previous condition will be replaced by the new one."
      )
    )
  }
  .env <- .dat2

  cnd <- eval_conditions(dat = dat, ..., .na = .na, .env = .env)
  new_cnd_df(dat, cnd = cnd, .warn = FALSE)
}

#' Mutate method for conditioned data frames
#'
#' [mutate.cnd_df()] is an S3 method to be dispatched by [mutate][dplyr::mutate]
#' generic on conditioned data frames. This function implements a conditional
#' mutate by only changing rows for which the condition stored in the
#' conditioned data frame is `TRUE`.
#'
#' @param .data A conditioned data frame.
#' @param .by Not used when `.data` is a conditioned data frame.
#' @param .before Not used, use `.after` instead.
#' @param .after Control where new columns should appear, i.e. after which
#'   columns.
#'
#' @inheritParams dplyr::mutate
#' @importFrom dplyr mutate
#' @return A conditioned data frame, meaning a tibble with mutated values.
#' @export
mutate.cnd_df <- function(.data,
                          ...,
                          .by = NULL,
                          .keep = c("all", "used", "unused", "none"),
                          .before = NULL,
                          .after = NULL) {
  if (!rlang::is_null(.by)) {
    cli::cli_abort("`.by` is not supported on conditioned data frames.")
  }

  if (!rlang::is_null(.before)) {
    cli::cli_abort("`.before` is not supported on conditioned data frames, use `.after` instead.")
  }

  cnd <- get_cnd_df_cnd(.data) # nolint object_name_linter()
  dat <- rm_cnd_df(.data) # avoids recursive S3 method dispatch.

  derivations <- rlang::enquos(...)
  derived_vars <- names(derivations)

  lst <- purrr::map(derivations, ~ rlang::expr(dplyr::if_else(!!cnd, !!.x, NA)))
  lst <- rlang::set_names(lst, derived_vars)

  dplyr::mutate(dat, !!!lst, .by = NULL, .keep = .keep, .after = dplyr::all_of(.after))
}
