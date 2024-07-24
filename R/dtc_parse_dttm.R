#' @rdname parse_dttm
#' @order 2
parse_dttm_ <- function(dttm,
                        fmt,
                        fmt_c = fmt_cmp(),
                        na = NULL,
                        sec_na = na,
                        min_na = na,
                        hour_na = na,
                        mday_na = na,
                        mon_na = na,
                        year_na = na) {
  admiraldev::assert_character_scalar(fmt)

  regex <-
    dttm_fmt_to_regex(
      fmt,
      fmt_regex = fmt_rg(
        na = na,
        sec_na = sec_na,
        min_na = min_na,
        hour_na = hour_na,
        mday_na = mday_na,
        mon_na = mon_na,
        year_na = year_na
      ),
      fmt_c = fmt_c
    )

  m <- stringr::str_match(dttm, regex)

  # Drop matching subgroups (those are unnamed)
  m <- m[, colnames(m) != "", drop = FALSE]

  complete_capture_matrix(m)
}

#' Parse a date, time, or date-time
#'
#' [parse_dttm()] extracts date and time components. [parse_dttm()] wraps around
#' [parse_dttm_()], which is not vectorized over `fmt`.
#'
#' @param dttm A character vector of dates, times or date-times.
#' @param fmt In the case of `parse_dttm()`, a character vector of parsing
#'   formats, or a single string format in the case of `parse_dttm_()`. When a
#'   character vector of formats is passed, each format is attempted in turn
#'   with the first parsing result to be successful taking precedence in the
#'   final result. The formats in `fmt` can be any strings, however the
#'   following characters (or successive repetitions thereof) are reserved in
#'   the sense that they are treated in a special way:
#'   - `"y"`: parsed as year;
#'   - `"m"`: parsed as month;
#'   - `"d"`: parsed as day;
#'   - `"H"`: parsed as hour;
#'   - `"M"`: parsed as minute;
#'   - `"S"`: parsed as second.
#'
#' @param na,sec_na,min_na,hour_na,mday_na,mon_na,year_na A character vector of
#'   alternative values to allow during matching. This can be used to indicate
#'   different forms of missing values to be found during the parsing date-time
#'   strings.
#'
#' @returns A character matrix of six columns: `"year"`, `"mon"`, `"mday"`,
#'   `"hour"`, `"min"` and `"sec"`. Each row corresponds to an element in
#'   `dttm`. Each element of the matrix is the parsed date/time component.
#'
#' @keywords internal
parse_dttm <- function(dttm,
                       fmt,
                       fmt_c = fmt_cmp(),
                       na = NULL,
                       sec_na = na,
                       min_na = na,
                       hour_na = na,
                       mday_na = na,
                       mon_na = na,
                       year_na = na) {
  lst <-
    purrr::map(
      fmt,
      ~ parse_dttm_(
        dttm = dttm,
        fmt = .x,
        fmt_c = fmt_c,
        na = na,
        sec_na = sec_na,
        min_na = min_na,
        hour_na = hour_na,
        mday_na = mday_na,
        mon_na = mon_na,
        year_na = year_na
      )
    )

  coalesce_capture_matrices(!!!lst)
}
