add_problems <- function(x, is_problem, dtc) {
  is_x_na <- is_problem
  if (!any(is_x_na)) {
    return(x)
  }

  names <- names(dtc)
  bad_names <- duplicated(names) | names == ""
  compat_names <- paste0("..var", seq_along(dtc))

  if (is.null(names)) {
    names <- compat_names
  } else {
    names[bad_names] <- compat_names[bad_names]
  }

  names(dtc) <- names

  index <- which(is_problem)
  problems <- tibble::as_tibble(dtc)[is_problem, ]
  problems <- tibble::add_column(problems, ..i = index, .before = 1L)
  attr(x, "problems") <- problems
  x
}

#' Detect problems with the parsing of date/times
#'
#' @description
#'
#' [any_problems()] takes a list of capture matrices (see [parse_dttm()]) and
#' reports on parsing problems by means of predicate values. A `FALSE` value
#' indicates the parsing was successful and a `TRUE` value that the parsing
#' failed in at least one of the inputs to [create_iso8601()]. Note that each
#' capture matrix corresponds to one input to [create_iso8601()].
#'
#' Note that this is an internal function to be used in the context of
#' [create_iso8601()] source code.
#'
#' @param cap_matrices A list of capture matrices in the sense of the returned
#' value by [parse_dttm()].
#' @param .cutoff_2000 An integer value. Two-digit years smaller or equal to
#'   `.cutoff_2000` are parsed as though starting with `20`, otherwise parsed as
#'   though starting with `19`.
#'
#' @returns A `logical` whose length matches the number underlying date/times
#'   passed as inputs to [create_iso8601()], i.e. whose length matches the
#'   number of rows of the capture matrices in `cap_matrices`.
#'
#' @examples
#' # No problem (return value is `FALSE`).
#' sdtm.oak:::any_problems(list(sdtm.oak:::parse_dttm("1980-06-18", "y-m-d")))
#'
#' # Now the parsing fails (return value is `TRUE`).
#' sdtm.oak:::any_problems(list(sdtm.oak:::parse_dttm("1980-06-18", "ymd")))
#'
#' # Find if there has been a problem in either in the `date` or `time` inputs.
#' # The following problems are expected with:
#' #   - `"2001/12/25"` as it won't be parsed with the format `"y-m-d"`
#' #   - `"00h12m21"` as it won't be parsed with the format `"H:M:S"`.
#' #
#' date <- c("2000-01-05", "2001/12/25", "1980-06-18", "1979-09-07")
#' time <- c("00h12m21", "22:35:05", "03:00:15", "07:09:00")
#'
#' cap_matrix_date <- sdtm.oak:::parse_dttm(date, "y-m-d")
#' cap_matrix_time <- sdtm.oak:::parse_dttm(time, "H:M:S")
#'
#' (cap_matrices <- list(cap_matrix_date, cap_matrix_time))
#'
#' # `any_problems()` returns `TRUE` for the first two elements because of the
#' # failure to parse `"2001/12/25"` and `"00h12m21"`, respectively.
#' sdtm.oak:::any_problems(cap_matrices)
#'
#' @keywords internal
any_problems <- function(cap_matrices, .cutoff_2000 = 68L) {
  cap_matrices |>
    purrr::map(~ format_iso8601(.x, .cutoff_2000 = .cutoff_2000)) |>
    unlist() |>
    matrix(ncol = length(cap_matrices)) |>
    is.na() |>
    rowSums() |>
    as.logical()
}

#' Retrieve date/time parsing problems
#'
#' [problems()] is a companion helper function to [create_iso8601()]. It
#' retrieves ISO 8601 parsing problems from an object of class iso8601, which is
#' [create_iso8601()]'s return value and that might contain a `problems`
#' attribute in case of parsing failures. [problems()] is a helper function that
#' provides easy access to these parsing problems.
#'
#' @param x An object of class iso8601, as typically obtained from a call to
#'   [create_iso8601()]. The argument can also be left empty, in that case it
#'   `problems()` will use the last returned value, making it convenient to use
#'   immediately after [create_iso8601()].
#'
#' @returns If there are no parsing problems in `x`, then the returned value is
#'   `NULL`; otherwise, a [tibble][tibble::tibble-package] of parsing failures
#'   is returned. Each row corresponds to a parsing problem. There will be a
#'   first column named `..i` indicating the position(s) in the inputs to the
#'   [create_iso8601()] call that resulted in failures; remaining columns
#'   correspond to the original input values passed on to [create_iso8601()],
#'   with columns being automatically named `..var1`, `..var2`, and so on, if
#'   the inputs to [create_iso8601()] were unnamed, otherwise, the original
#'   variable names are used instead.
#'
#' @examples
#' dates <-
#'   c(
#'     "2020-01-01",
#'     "2020-02-11",
#'     "2020-01-06",
#'     "2020-0921",
#'     "2020/10/30",
#'     "2020-12-05",
#'     "20231225"
#'   )
#'
#' #' # By inspecting the problematic dates it can be understood that
#' # the `.format` parameter needs to updated to include other variations.
#' iso8601_dttm <- create_iso8601(dates, .format = "y-m-d")
#' problems(iso8601_dttm)
#'
#' # Including more parsing formats addresses the previous problems
#' formats <- c("y-m-d", "y-md", "y/m/d", "ymd")
#' iso8601_dttm2 <- create_iso8601(dates, .format = list(formats))
#'
#' # So now `problems()` returns `NULL` because there are no more parsing issues.
#' problems(iso8601_dttm2)
#'
#' # If you pass named arguments when calling `create_iso8601()` then they will
#' # be used to create the problems object.
#' iso8601_dttm3 <- create_iso8601(date = dates, .format = "y-m-d")
#' problems(iso8601_dttm3)
#'
#' @export
problems <- function(x = .Last.value) {
  probs <- attr(x, "problems")
  if (!is.null(probs)) {
    probs
  } else {
    invisible(NULL)
  }
}

n_problems <- function(x) {
  probs <- problems(x)
  if (is.null(probs)) {
    return(0L)
  } else {
    nrow(probs)
  }
}

warn_problems <- function(x) {
  n_probs <- n_problems(x)
  if (n_probs > 0L) {
    msg <- paste(
      sprintf("There were %d parsing problems.", n_probs),
      "Run `problems()` on parsed results for details."
    )
    rlang::warn(msg)
  }

  invisible(NULL)
}
