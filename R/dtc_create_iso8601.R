# Month abbreviation (en) to numeric month mapping
mon_abb_to_mon_num <- stats::setNames(sprintf("%02d", seq_along(month.abb)), tolower(month.abb))

#' Convert NA to `"-"`
#'
#' [iso8601_na()] takes a character vector and converts `NA` values to `"-"`.
#'
#' @param x A character vector.
#'
#' @returns A character vector.
#'
#' @keywords internal
iso8601_na <- function(x) {
  admiraldev::assert_character_vector(x)
  x[is.na(x)] <- "-"
  x
}

#' Convert an integer to a zero-padded character vector
#'
#' [zero_pad_whole_number()] takes non-negative integer values and converts
#' them to character with zero padding. Negative numbers and numbers greater
#' than the width specified by the number of digits `n` are converted to `NA`.
#'
#' @param x An integer vector.
#' @param n Number of digits in the output, including zero padding.
#'
#' @returns A character vector.
#'
#' @keywords internal
zero_pad_whole_number <- function(x, n = 2L) {
  # Check `x`
  if (!rlang::is_integerish(x)) cli::cli_abort("`x` must be integerish.")

  # Check `n`
  admiraldev::assert_integer_scalar(n)
  if (n < 1L) cli::cli_abort("`n` must be positive.")

  # Negative numbers are not allowed, and hence get converted to NA.
  x[x < 0L] <- NA_integer_

  # Numbers that do not fit within the padding width are converted to NA
  x[floor(log10(x)) >= n] <- NA_integer_

  fmt <- paste0("%0", n, "d")
  y <- sprintf(fmt, x)
  y[is.na(x)] <- NA_character_
  y
}

#' Convert two-digit to four-digit years
#'
#' [yy_to_yyyy()] converts two-digit years to four-digit years.
#'
#' @param x An integer vector of years.
#' @param cutoff_2000 An integer value. Two-digit years smaller or equal to
#'   `cutoff_2000` are parsed as though starting with `20`, otherwise parsed as
#'   though starting with `19`.
#'
#' @returns An integer vector.
#'
#' @keywords internal
yy_to_yyyy <- function(x, cutoff_2000 = 68L) {
  # Check `x`
  if (!rlang::is_integerish(x)) cli::cli_abort("`x` must be integerish.")

  if (any(x < 0L, na.rm = TRUE)) {
    cli::cli_abort("`x` cannot have negative years.")
  }

  x <- dplyr::if_else(x <= cutoff_2000, x + 2000L, x)
  x <- dplyr::if_else(x <= 99L, x + 1900L, x)
  x
}

#' Format as a ISO8601 two-digit number
#'
#' [iso8601_two_digits()] converts a single digit or two digit number into a
#' two digit, 0-padded, number. Failing to parse the input as a two digit number
#' results in `NA`.
#'
#' @param x A character vector.
#'
#' @returns A character vector of the same size as `x`.
#'
#' @keywords internal
iso8601_two_digits <- function(x) {
  admiraldev::assert_character_vector(x)
  x_int <- as.integer(stringr::str_match(x, "^\\d?\\d$"))
  zero_pad_whole_number(x_int, n = 2L)
}

iso8601_mday <- iso8601_two_digits
iso8601_hour <- iso8601_two_digits
iso8601_min <- iso8601_two_digits

#' Format as a ISO8601 four-digit year
#'
#' [iso8601_year()] converts a character vector whose values represent years to
#' four-digit years.
#'
#' @param x A character vector.
#' @param cutoff_2000 A non-negative integer value. Two-digit years smaller or
#'   equal to `cutoff_2000` are parsed as though starting with `20`, otherwise
#'   parsed as though starting with `19`.
#'
#' @returns A character vector.
#'
#' @keywords internal
iso8601_year <- function(x, cutoff_2000 = 68L) {
  admiraldev::assert_character_vector(x)
  admiraldev::assert_integer_scalar(cutoff_2000, subset = "non-negative")
  x_int <- as.integer(stringr::str_match(x, "^\\d{1,4}$"))
  x_int <- yy_to_yyyy(x_int, cutoff_2000 = cutoff_2000)
  zero_pad_whole_number(x_int, n = 4L)
}

#' Format as a ISO8601 month
#'
#' [iso8601_mon()] converts a character vector whose values represent numeric
#' or abbreviated month names to zero-padded numeric months.
#'
#' @param x A character vector.
#'
#' @returns A character vector.
#'
#' @keywords internal
iso8601_mon <- function(x) {
  x <- tolower(x)
  num_mon <- mon_abb_to_mon_num[x]
  num_mon_chr <- num_mon
  num_mon_chr[is.na(num_mon)] <- iso8601_two_digits(x[is.na(num_mon)])
  mon_int <- as.integer(num_mon_chr)
  zero_pad_whole_number(mon_int, n = 2L)
}

#' Format as ISO8601 seconds
#'
#' [iso8601_sec()] converts a character vector whose values represent seconds.
#'
#' @param x A character vector.
#'
#' @returns A character vector.
#'
#' @keywords internal
iso8601_sec <- function(x) {
  x_iso8601 <- stringr::str_extract(x, "^\\d?\\d(\\.\\d*)?$")
  x_iso8601 <- stringr::str_replace(x_iso8601, "^\\d(\\.\\d*)?$", "0\\0")
  x_iso8601 <- stringr::str_replace(x_iso8601, "(\\.[^0]*)(0*)$", "\\1")
  x_iso8601 <- stringr::str_remove(x_iso8601, "\\.$")
  x_iso8601[is.na(x_iso8601)] <- NA_character_
  x_iso8601
}

#' Truncate a partial ISO8601 date-time
#'
#' [iso8601_truncate()] converts a character vector of ISO8601 dates, times or
#' date-times that might be partial and truncates the format by removing those
#' missing components.
#'
#' @param x A character vector.
#'
#' @returns A character vector.
#' @keywords internal
iso8601_truncate <- function(x, empty_as_na = TRUE) {
  x <- stringr::str_remove(x, "[^\\d]*$")
  if (empty_as_na) x[x == ""] <- NA_character_
  x
}

#' Convert date/time components into ISO8601 format
#'
#' [format_iso8601()] takes a character matrix of date/time components and
#' converts each component to ISO8601 format. In practice this entails
#' converting years to a four digit number, and month, day, hours, minutes and
#' seconds to two-digit numbers. Not available (`NA`) components are converted
#' to `"-"`.
#'
#' @param m A character matrix of date/time components. It must have six
#'   named columns: `year`, `mon`, `mday`, `hour`, `min` and `sec`.
#' @param .cutoff_2000 An integer value. Two-digit years smaller or equal to
#'   `.cutoff_2000` are parsed as though starting with `20`, otherwise parsed as
#'   though starting with `19`.
#'
#' @returns A character vector with date-times following the ISO8601 format.
#'
#' @keywords internal
format_iso8601 <- function(m, .cutoff_2000 = 68L) {
  admiraldev::assert_integer_scalar(.cutoff_2000)

  m[, "year"] <- iso8601_year(m[, "year"], cutoff_2000 = .cutoff_2000)
  m[, "mon"] <- iso8601_mon(m[, "mon"])
  m[, "mday"] <- iso8601_mday(m[, "mday"])
  m[, "hour"] <- iso8601_hour(m[, "hour"])
  m[, "min"] <- iso8601_min(m[, "min"])
  m[, "sec"] <- iso8601_sec(m[, "sec"])

  m <- iso8601_na(m)

  x <-
    paste0(
      m[, "year"],
      "-",
      m[, "mon"],
      "-",
      m[, "mday"],
      "T",
      m[, "hour"],
      ":",
      m[, "min"],
      ":",
      m[, "sec"]
    )

  iso8601_truncate(x)
}

#' Convert date or time collected values to ISO 8601
#'
#' [create_iso8601()] converts vectors of dates, times or date-times to [ISO
#' 8601](https://en.wikipedia.org/wiki/ISO_8601) format. Learn more in
#' `vignette("iso_8601")`.
#'
#' @param ... Character vectors of dates, times or date-times' components.
#' @param .format Parsing format(s). Either a character vector or a list of
#'   character vectors. If a character vector is passed then each element is
#'   taken as parsing format for each vector passed in `...`. If a list is
#'   provided, then each element must be a character vector of formats. The
#'   first vector of formats is used for parsing the first vector passed in
#'   `...`, and so on.
#' @param .fmt_c A list of regexps to use when parsing `.format`. Use [fmt_cmp()]
#' to create such an object to pass as argument to this parameter.
#' @param .na A character vector of string literals to be regarded as missing
#'   values during parsing.
#' @param .cutoff_2000 An integer value. Two-digit years smaller or equal to
#'   `.cutoff_2000` are parsed as though starting with `20`, otherwise parsed as
#'   though starting with `19`.
#' @param .check_format Whether to check the formats passed in `.format`,
#'   meaning to check against a selection of validated formats in
#'   [dtc_formats][sdtm.oak::dtc_formats]; or to have a more permissible
#'   interpretation of the formats.
#' @param .warn Whether to warn about parsing failures.
#'
#' @return A vector of dates, times or date-times in [ISO
#' 8601](https://en.wikipedia.org/wiki/ISO_8601) format
#'
#' @examples
#' # Converting dates
#' create_iso8601(c("2020-01-01", "20200102"), .format = "y-m-d")
#' create_iso8601(c("2020-01-01", "20200102"), .format = "ymd")
#' create_iso8601(c("2020-01-01", "20200102"), .format = list(c("y-m-d", "ymd")))
#'
#' # Two-digit years are supported
#' create_iso8601(c("20-01-01", "200101"), .format = list(c("y-m-d", "ymd")))
#'
#' # `.cutoff_2000` sets the cutoff for two-digit to four-digit year conversion
#' # Default is at 68.
#' create_iso8601(c("67-01-01", "68-01-01", "69-01-01"), .format = "y-m-d")
#'
#' # Change it to 80.
#' create_iso8601(c("79-01-01", "80-01-01", "81-01-01"), .format = "y-m-d", .cutoff_2000 = 80)
#'
#' # Converting times
#' create_iso8601("15:10", .format = "HH:MM")
#' create_iso8601("2:10", .format = "HH:MM")
#' create_iso8601("2:1", .format = "HH:MM")
#' create_iso8601("02:01:56", .format = "HH:MM:SS")
#' create_iso8601("020156.5", .format = "HHMMSS")
#'
#' # Converting date-times
#' create_iso8601("12 NOV 202015:15", .format = "dd mmm yyyyHH:MM")
#'
#' # Indicate allowed missing values to make the parsing pass
#' create_iso8601("U DEC 201914:00", .format = "dd mmm yyyyHH:MM")
#' create_iso8601("U DEC 201914:00", .format = "dd mmm yyyyHH:MM", .na = "U")
#'
#' create_iso8601("NOV 2020", .format = "m y")
#' create_iso8601(c("MAR 2019", "MaR 2020", "mar 2021"), .format = "m y")
#'
#' create_iso8601("2019-04-041045-", .format = "yyyy-mm-ddHHMM-")
#'
#' create_iso8601("20200507null", .format = "ymd(HH:MM:SS)")
#' create_iso8601("20200507null", .format = "ymd((HH:MM:SS)|null)")
#'
#' # Fractional seconds
#' create_iso8601("2019-120602:20:13.1230001", .format = "y-mdH:M:S")
#'
#' # Use different reserved characters in the format specification
#' # Here we change "H" to "x" and "M" to "w", for hour and minute, respectively.
#' create_iso8601("14H00M", .format = "HHMM")
#' create_iso8601("14H00M", .format = "xHwM", .fmt_c = fmt_cmp(hour = "x", min = "w"))
#'
#' # Alternative formats with unknown values
#' datetimes <- c("UN UNK 201914:00", "UN JAN 2021")
#' format <- list(c("dd mmm yyyy", "dd mmm yyyyHH:MM"))
#' create_iso8601(datetimes, .format = format, .na = c("UN", "UNK"))
#'
#' # Dates and times may come in many format variations
#' fmt <- "dd MMM yyyy HH nn ss"
#' fmt_cmp <- fmt_cmp(mon = "MMM", min = "nn", sec = "ss")
#' create_iso8601("05 feb 1985 12 55 02", .format = fmt, .fmt_c = fmt_cmp)
#'
#' @export
create_iso8601 <-
  function(...,
           .format,
           .fmt_c = fmt_cmp(),
           .na = NULL,
           .cutoff_2000 = 68L,
           .check_format = FALSE,
           .warn = TRUE) {
    assert_fmt_c(.fmt_c)
    admiraldev::assert_logical_scalar(.check_format)
    admiraldev::assert_logical_scalar(.warn)

    dots <- rlang::dots_list(...)

    if (rlang::is_empty(dots)) {
      return(character())
    }

    # Check if all vectors in `dots` are of character type.
    if (!identical(unique(sapply(dots, typeof)), "character")) {
      cli::cli_abort("All vectors in `...` must be of type character.")
    }

    # Check if all vectors in `dots` are of the same length.
    n <- unique(lengths(dots))
    if (!identical(length(n), 1L)) {
      cli::cli_abort("All vectors in `...` must be of the same length.")
    }

    if (!identical(length(dots), length(.format))) {
      cli::cli_abort("Number of vectors in `...` should match length of `.format`.")
    }

    # Check that the `.format` is either a character vector or a list of
    # character vectors, and that each string is one of the possible formats.
    if (.check_format) {
      assert_dtc_format(.format)
    }

    cap_matrices <-
      purrr::map2(
        dots,
        .format,
        ~ parse_dttm(
          dttm = .x,
          fmt = .y,
          na = .na,
          fmt_c = .fmt_c
        )
      )
    cap_matrix <- coalesce_capture_matrices(!!!cap_matrices)

    iso8601 <- format_iso8601(cap_matrix, .cutoff_2000 = .cutoff_2000)
    any_prob <- any_problems(cap_matrices, .cutoff_2000 = .cutoff_2000)
    iso8601 <- add_problems(iso8601, any_prob, dots)
    class(iso8601) <- "iso8601"

    if (.warn && rlang::is_interactive()) {
      warn_problems(iso8601)
    }

    iso8601
  }

#' @export
print.iso8601 <- function(x, ...) {
  # Here we take advantage of the subset operator `[` dropping
  # attributes. Also, using `seq_along()` should not force a copy of `x` thus
  # being memory-efficient.
  print(x[seq_along(x)])
  invisible(x)
}
