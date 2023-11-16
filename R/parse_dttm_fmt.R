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

  if (!rlang::is_integerish(x))
    rlang::abort("`x` must be integer-ish")

  if (rlang::is_empty(x))
    return(tibble::tibble(start = integer(), end = integer()))

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
  if (identical(case, "lower")) x <-  stringr::str_to_lower(x)
  if (identical(case, "title")) x <- stringr::str_to_title(x)

  stringr::str_flatten(x, collapse = "|")
}


# Date time components. This is a nice
# utility function that allows you to easily
# change the regexp for one specific dttm component
# while keeping the other defaults.

#' Regexps for date/time format components
#'
#' [fmt_c()] creates a character vector of patterns to match individual
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
#' # Default patterns
#' sdtm.oak:::fmt_c()
#'
#' @keywords internal
fmt_c <- function(sec = "S+",
                  min = "M+",
                  hour = "H+",
                  mday = "d+",
                  mon = "m+",
                  year = "y+") {

  c(
    sec = sec,
    min = min,
    hour = hour,
    mday = mday,
    mon = mon,
    year = year
  )

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
#' @examples
#' # A regex for matching either "jan" or "feb"
#' sdtm.oak:::regex_or(c("jan", "feb"))
#'
#' # Setting `.open` and/or `.close` to `TRUE` can be handy if this regex
#' # is to be combined into a larger regex.
#' paste0(sdtm.oak:::regex_or(c("jan", "feb"), .close = TRUE), r"{\d{2}}")
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
#' @examples
#' # Default regexps
#' sdtm.oak:::fmt_rg()
#'
#' # You may change the way months are matched, e.g. you might not want to match
#' # month abbreviations, i.e. only numerical months. So pass an explicit regex
#' # for numerical months:
#' sdtm.oak:::fmt_rg(mon = r"[\b\d|\d{2}]")
#'
#' # Make date/time components accept `"UNK"` as a possible pattern (useful
#' # to match funny codes for `NA`).
#' sdtm.oak:::fmt_rg(na = "UNK")
#'
#' # Or be more specific and use `"UNK"` for the year component only.
#' sdtm.oak:::fmt_rg(year_na = "UNK")
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

# Scalar version of `parse_dttm_fmt()`.
parse_dttm_fmt_ <- function(x, pattern) {

  match_data <- regexpr(pattern, x)
  match <- reg_matches(x, match_data)

  is_match <- !is.na(match)

  start <- ifelse(is_match, match_data, NA_integer_)
  len <- ifelse(is_match, attr(match_data, "match.length"), NA_integer_)
  end <- start + len - 1L
  tibble::tibble(pat = pattern, cap = match, start = start, end = end, len = len)
}

parse_dttm_fmt <- function(fmt, patterns = fmt_c()) {

  fmt_dttmc <-
    purrr::map(patterns, ~ parse_dttm_fmt_(fmt, .x)) |>
    purrr::list_rbind(names_to = "fmt_c")

  # Get captures' ranks while leaving NA as NA (`rank()` won't do this.)
  fmt_dttmc$ord <- dplyr::row_number(fmt_dttmc$start)

  fmt_len <- nchar(fmt)

  start <- end <- NULL # To avoid a "no visible binding for global variable" NOTE.
  dttmc_pos <- with(fmt_dttmc, pseq(from = start[!is.na(start)], to = end[!is.na(end)]))
  # `delim_pos`: delimiter positions, i.e. positions in `fmt` in-between dttm components.
  delim_pos <- find_int_gap(dttmc_pos, xmin = 1L, xmax = fmt_len)

  delim <- with(delim_pos, stringr::str_sub(fmt, start = start, end = end))
  fmt_delim <-
    tibble::tibble(
      fmt_c = NA_character_,
      pat = NA_character_,
      cap = delim,
      start = delim_pos$start,
      end = delim_pos$end,
      len = end - start + 1L
    )

  dplyr::bind_rows(fmt_dttmc, fmt_delim) |>
    dplyr::arrange(.data$start)

}

dttm_fmt_to_regex <- function(tbl_fmt_c, fmt_regex = fmt_rg(), anchored = TRUE) {
  fmt_regex <-
    tbl_fmt_c |>
    dplyr::mutate(regex = dplyr::if_else(is.na(.data$fmt_c), .data$cap, fmt_regex[.data$fmt_c])) |>
    dplyr::mutate(regex = dplyr::if_else(is.na(.data$cap), NA_character_, .data$regex)) |>
    dplyr::pull(.data$regex)

  fmt_regex <- stringr::str_flatten(fmt_regex, na.rm = TRUE)
  if (anchored) fmt_regex <- stringr::str_glue("^{fmt_regex}$")

  fmt_regex
}
