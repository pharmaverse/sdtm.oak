#' Find gap intervals in integer sequences
#'
#' [find_int_gap()] determines the `start` and `end` positions for gap intervals
#' in a sequence of integers. By default, the interval range to look for gaps is
#' defined by the minimum and maximum values of `x`; specify `xmin` and `xmax`
#' to change the range explicitly.
#'
#' @param x An integer vector.
#' @param xmin Left endpoint integer value.
#' @param xmax Right endpoint integer value.
#'
#' @returns A [tibble][tibble::tibble-package] of gap intervals of two columns:
#' - `start`: left endpoint
#' - `end`: right endpoint
#' If no gap intervals are found then an empty [tibble][tibble::tibble-package]
#' is returned.
#'
#' @keywords internal
find_int_gap <- function(x, xmin = min(x), xmax = max(x)) {
  if (!rlang::is_integerish(x)) {
    cli::cli_abort("`x` must be integer-ish")
  }

  if (rlang::is_empty(x)) {
    return(tibble::tibble(start = integer(), end = integer()))
  }

  admiraldev::assert_integer_scalar(xmin)
  admiraldev::assert_integer_scalar(xmax)

  x <- sort(unique(x))
  x <- c(xmin - 1L, x, xmax + 1L)
  gaps <- which(diff(x) > 1L)
  start <- x[gaps] + 1L
  end <- x[gaps + 1L] - 1L
  tibble::tibble(start = start, end = end)
}

#' `regmatches()` with `NA`
#'
#' [reg_matches()] is a thin wrapper around [regmatches()] that returns
#' `NA` instead of `character(0)` when matching fails.
#'
#' @param x A character vector.
#' @param m An object with match data.
#' @param invert A logical scalar. If `TRUE`, extract or replace the non-matched
#'   substrings.
#'
#' @returns A list of character vectors with the matched substrings, or `NA` if
#'   matching failed.
#'
#' @keywords internal
reg_matches <- function(x, m, invert = FALSE) {
  match <- regmatches(x, m, invert = invert)
  match[!lengths(match)] <- NA_character_
  match
}

#' Parallel sequence generation
#'
#' [pseq()] is similar to [seq()] but conveniently accepts integer vectors as
#' inputs to `from` and `to`, allowing for parallel generation of sequences.
#' The result is the union of the generated sequences.
#'
#' @param from An integer vector. The starting value(s) of the sequence(s).
#' @param to An integer vector. The ending value(s) of the sequence(s).
#'
#' @returns An integer vector.
#'
#' @keywords internal
pseq <- function(from, to) {
  unlist(purrr::map2(.x = from, .y = to, .f = `:`))
}

#' Generate case insensitive regexps
#'
#' [str_to_anycase()] takes a character vector of word strings as input, and
#' generates regular expressions that express that match in any case.
#'
#' @param x A character vector of strings consisting of word characters.
#'
#' @returns A character vector.
#'
#' @keywords internal
str_to_anycase <- function(x) {
  lst <- stringr::str_split(x, stringr::boundary("character"))
  purrr::map(lst, ~ stringr::str_c(stringr::str_to_upper(.x), stringr::str_to_lower(.x))) |>
    purrr::map(~ sprintf("[%s]", .x)) |>
    purrr::map(~ stringr::str_flatten(.x)) |>
    unlist()
}

#' Regex for months' abbreviations
#'
#' [months_abb_regex()] generates a regex that matches month abbreviations. For
#' finer control, the case can be specified with parameter `case`.
#'
#' @param x A character vector of three-letter month abbreviations. Default is
#'   `month.abb`.
#' @param case A string scalar: `"any"`, if month abbreviations are to be
#'   matched in any case; `"upper"`, to match uppercase abbreviations;
#'   `"lower"`, to match lowercase; and, `"title"` to match title case.
#'
#' @returns A regex as a string.
#'
#' @keywords internal
months_abb_regex <- function(x = month.abb, case = c("any", "upper", "lower", "title")) {
  admiraldev::assert_character_vector(x)
  case <- match.arg(case)

  if (identical(case, "any")) x <- str_to_anycase(x)
  if (identical(case, "upper")) x <- stringr::str_to_upper(x)
  if (identical(case, "lower")) x <- stringr::str_to_lower(x)
  if (identical(case, "title")) x <- stringr::str_to_title(x)

  stringr::str_flatten(x, collapse = "|")
}


# Date time components. This is a nice
# utility function that allows you to easily
# change the regexp for one specific dttm component
# while keeping the other defaults.

#' Regexps for date/time format components
#'
#' [fmt_cmp()] creates a character vector of patterns to match individual
#' format date/time components.
#'
#' @param sec A string pattern for matching the second format component.
#' @param min A string pattern for matching the minute format component.
#' @param hour A string pattern for matching the hour format component.
#' @param mday A string pattern for matching the month day format component.
#' @param mon A string pattern for matching the month format component.
#' @param year A string pattern for matching the year format component.
#'
#' @returns A named character vector of date/time format patterns. This a vector
#' of six elements, one for each date/time component.
#'
#' @examples
#' # Regexps to parse format components
#' fmt_cmp()
#'
# # Supply a different pattern for the year component
#' fmt_cmp(year = "yyyy")
#'
#' @export
fmt_cmp <- function(sec = "S+",
                    min = "M+",
                    hour = "H+",
                    mday = "d+",
                    mon = "m+",
                    year = "y+") {
  structure(
    list(
      sec = sec,
      min = min,
      hour = hour,
      mday = mday,
      mon = mon,
      year = year
    ),
    class = "fmt_c"
  )
}

assert_fmt_c <- function(x) {
  if (!inherits(x, "fmt_c")) {
    cli::cli_abort("`x` must be an object created with `fmt_cmp()`.")
  }

  invisible(x)
}

#' Utility function to assemble a regex of alternative patterns
#'
#' [regex_or()] takes a set of patterns and binds them with the Or (`"|"`)
#' pattern for an easy regex of alternative patterns.
#'
#' @param x A character vector of alternative patterns.
#' @param .open Whether the resulting regex should start with `"|"`.
#' @param .close Whether the resulting regex should end with `"|"`.
#'
#' @returns A character scalar of the resulting regex.
#'
#' @keywords internal
regex_or <- function(x, .open = FALSE, .close = FALSE) {
  admiraldev::assert_character_vector(x)
  admiraldev::assert_logical_scalar(.open)
  admiraldev::assert_logical_scalar(.close)

  if (.open) x <- c("", x)
  if (.close) x <- c(x, "")

  stringr::str_flatten(x, collapse = "|")
}

#' Regexps for date/time components
#'
#' [fmt_rg()] creates a character vector of named patterns to match individual
#' date/time components.
#'
#' @param sec Regexp for the second component.
#' @param min Regexp for the minute component.
#' @param hour Regexp for the hour component.
#' @param mday Regexp for the month day component.
#' @param mon Regexp for the month component.
#' @param year Regexp for the year component.
#' @param na Regexp of alternatives, useful to match special values coding for
#' missingness.
#' @param sec_na Same as `na` but specifically for the second component.
#' @param min_na Same as `na` but specifically for the minute component.
#' @param hour_na Same as `na` but specifically for the hour component.
#' @param mday_na Same as `na` but specifically for the month day component.
#' @param mon_na Same as `na` but specifically for the month component.
#' @param year_na Same as `na` but specifically for the year component.
#'
#' @returns A named character vector of named patterns (regexps) for matching
#'   each date/time component.
#'
#' @keywords internal
fmt_rg <- function(
    sec = r"[(\b\d|\d{2})(\.\d*)?]",
    min = r"[(\b\d|\d{2})]",
    hour = r"[\d?\d]",
    mday = r"[\b\d|\d{2}]",
    mon = stringr::str_glue(r"[\d\d|{months_abb_regex()}]"),
    year = r"[(\d{2})?\d{2}]",
    na = NULL,
    sec_na = na,
    min_na = na,
    hour_na = na,
    mday_na = na,
    mon_na = na,
    year_na = na) {
  sec_na <-
    ifelse(!is.null(sec_na), regex_or(sec_na, .open = TRUE), "")
  min_na <-
    ifelse(!is.null(min_na), regex_or(min_na, .open = TRUE), "")
  hour_na <-
    ifelse(!is.null(hour_na), regex_or(hour_na, .open = TRUE), "")
  mday_na <-
    ifelse(!is.null(mday_na), regex_or(mday_na, .open = TRUE), "")
  mon_na <-
    ifelse(!is.null(mon_na), regex_or(mon_na, .open = TRUE), "")
  year_na <-
    ifelse(!is.null(year_na), regex_or(year_na, .open = TRUE), "")


  c(
    sec = stringr::str_glue("(?<sec>{sec}{sec_na})"),
    min = stringr::str_glue("(?<min>{min}{min_na})"),
    hour = stringr::str_glue("(?<hour>{hour}{hour_na})"),
    mday = stringr::str_glue("(?<mday>{mday}{mday_na})"),
    mon = stringr::str_glue("(?<mon>{mon}{mon_na})"),
    year = stringr::str_glue("(?<year>{year}{year_na})")
  )
}

fmt_dttmc <-
  function(fmt_c = character(),
           pat = character(),
           cap = character(),
           start = integer(),
           end = integer(),
           len = integer(),
           ord = integer()) {
    tibble::tibble(
      fmt_c = fmt_c,
      pat = pat,
      cap = cap,
      start = start,
      end = end,
      len = len,
      ord = ord
    )
  }

#' @rdname parse_dttm_fmt
parse_dttm_fmt_ <- function(fmt, pattern) {
  admiraldev::assert_character_scalar(fmt)
  admiraldev::assert_character_scalar(pattern)

  if (identical(nchar(pattern), 0L)) {
    cli::cli_abort("`pattern` must be a literal string of at least one char.")
  }

  match_data <- regexpr(pattern, fmt)
  match <- reg_matches(fmt, match_data)

  is_match <- (!length(match)) || (!is.na(match))

  start <- ifelse(is_match, match_data, NA_integer_)
  len <- ifelse(is_match, attr(match_data, "match.length"), NA_integer_)
  end <- start + len - 1L
  tibble::tibble(pat = pattern, cap = match, start = start, end = end, len = len)
}

#' Parse a date/time format
#'
#' [parse_dttm_fmt()] parses a date/time formats, meaning it will try to parse
#' the components of the format `fmt` that refer to date/time components.
#' [parse_dttm_fmt_()] is similar to [parse_dttm_fmt()] but is not vectorized
#' over `fmt`.
#'
#' @param fmt A format string (scalar) to be parsed by `patterns`.
#' @param pattern,patterns A string (in the case of `pattern`), or a character
#'   vector (in the case of `patterns`) of regexps for each of the individual
#'   date/time components. Default value is that of [fmt_cmp()]. Use this function
#'   if you plan on passing a different set of patterns.
#'
#' @returns A [tibble][tibble::tibble-package] of seven columns:
#' - `fmt_c`: date/time format component. Values are either `"year"`, `"mon"`,
#' `"mday"`, `"hour"`, `"min"`, `"sec"`, or `NA`.
#' - `pat`: Regexp used to parse the date/time component.
#' - `cap`: The captured substring from the format.
#' - `start`: Start position in the format string for this capture.
#' - `end`: End position in the format string for this capture.
#' - `len`: Length of the capture (number of chars).
#' - `ord`: Ordinal of this date/time component in the format string.
#'
#' Each row is for either a date/time format component or a "delimiter" string
#' or pattern in-between format components.
#'
#' @keywords internal
parse_dttm_fmt <- function(fmt, patterns = fmt_cmp()) {
  admiraldev::assert_character_scalar(fmt)

  fmt_dttmc <-
    purrr::map(patterns, ~ parse_dttm_fmt_(fmt, .x)) |>
    purrr::list_rbind(names_to = "fmt_c")

  # Check if patterns have matching overlap, i.e. whether they are not
  # mutually exclusive (as they should).
  if (anyDuplicated(pseq(fmt_dttmc$start, fmt_dttmc$end))) {
    cli::cli_abort("Patterns in `fmt_c` have overlapping matches.")
  }

  # Get captures' ranks while leaving NA as NA (`rank()` won't do this.)
  fmt_dttmc$ord <- dplyr::row_number(fmt_dttmc$start)

  if (identical(nrow(fmt_dttmc), 0L)) {
    return(fmt_dttmc())
  }

  fmt_len <- nchar(fmt)

  dttmc_pos <-
    pseq(from = fmt_dttmc$start[!is.na(fmt_dttmc$start)], to = fmt_dttmc$end[!is.na(fmt_dttmc$end)])
  # `delim_pos`: delimiter positions, i.e. positions in `fmt` in-between dttm components.
  delim_pos <- find_int_gap(dttmc_pos, xmin = 1L, xmax = fmt_len)

  delim <- with(delim_pos, stringr::str_sub(fmt, start = start, end = end))
  fmt_delim <-
    fmt_dttmc(
      fmt_c = NA_character_,
      pat = NA_character_,
      cap = delim,
      start = delim_pos$start,
      end = delim_pos$end,
      len = delim_pos$end - delim_pos$start + 1L,
      ord = NA_integer_
    )

  dplyr::bind_rows(fmt_dttmc, fmt_delim) |>
    dplyr::arrange(.data$start)
}

#' Convert a parsed date/time format to regex
#'
#' [dttm_fmt_to_regex()] takes a [tibble][tibble::tibble-package] of parsed
#' date/time format components (as returned by [parse_dttm_fmt()]), and a
#' mapping of date/time component formats to regexps and generates a single
#' regular expression with groups for matching each of the date/time components.
#'
#' @param fmt A format string (scalar) to be parsed by `patterns`.
#' @param fmt_regex A named character vector of regexps, one for each date/time
#' component.
#' @param anchored Whether the final regex should be anchored, i.e. bounded by
#' `"^"` and `"$"` for a whole match.
#'
#' @returns A string containing a regular expression for matching date/time
#' components according to a format.
#'
#' @keywords internal
dttm_fmt_to_regex <- function(fmt, fmt_regex = fmt_rg(), fmt_c = fmt_cmp(), anchored = TRUE) {
  tbl_fmt_c <- parse_dttm_fmt(fmt, patterns = fmt_c)

  fmt_regex <-
    tbl_fmt_c |>
    dplyr::mutate(regex = dplyr::if_else(is.na(.data$fmt_c), .data$cap, fmt_regex[.data$fmt_c])) |>
    dplyr::mutate(regex = dplyr::if_else(is.na(.data$cap), NA_character_, .data$regex)) |>
    dplyr::pull(.data$regex)

  fmt_regex <- stringr::str_flatten(fmt_regex, na.rm = TRUE)
  if (anchored) fmt_regex <- stringr::str_glue("^{fmt_regex}$")

  fmt_regex
}
