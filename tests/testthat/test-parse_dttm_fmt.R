test_that("`parse_dttm_fmt_`: empty fmt", {
  x <-
    tibble::tibble(
      pat = character(),
      cap = character(),
      start = integer(),
      end = integer(),
      len = integer()
    )
  expect_identical(x, parse_dttm_fmt_("", pattern = "y"))
  expect_error(parse_dttm_fmt_(character(), pattern = "y"))
})

test_that("`parse_dttm_fmt_`: empty pattern", {
  expect_error(parse_dttm_fmt_("ymd", pattern = ""))
  expect_error(parse_dttm_fmt_("ymd", pattern = character()))
})

test_that("`parse_dttm_fmt_`: basic usage", {
  fmt1 <- "y m d"
  fmt2 <- "y-m-d"

  x1 <-
    tibble::tibble(
      pat = "y",
      cap = "y",
      start = 1L,
      end = 1L,
      len = 1L
    )
  expect_identical(x1, parse_dttm_fmt_(fmt1, pattern = "y"))
  expect_identical(x1, parse_dttm_fmt_(fmt2, pattern = "y"))

  x2 <-
    tibble::tibble(
      pat = "m",
      cap = "m",
      start = 3L,
      end = 3L,
      len = 1L
    )
  expect_identical(x2, parse_dttm_fmt_(fmt1, pattern = "m"))
  expect_identical(x2, parse_dttm_fmt_(fmt2, pattern = "m"))

  x3 <-
    tibble::tibble(
      pat = "d",
      cap = "d",
      start = 5L,
      end = 5L,
      len = 1L
    )

  expect_identical(x3, parse_dttm_fmt_(fmt1, pattern = "d"))
  expect_identical(x3, parse_dttm_fmt_(fmt2, pattern = "d"))
})

test_that("`parse_dttm_fmt_`: pattern variations", {
  fmt <- "HH:MM:SS"

  x1 <-
    tibble::tibble(
      pat = "H",
      cap = "H",
      start = 1L,
      end = 1L,
      len = 1L
    )

  x2 <-
    tibble::tibble(
      pat = "HH",
      cap = "HH",
      start = 1L,
      end = 2L,
      len = 2L
    )

  x3 <-
    tibble::tibble(
      pat = "H+",
      cap = "HH",
      start = 1L,
      end = 2L,
      len = 2L
    )

  expect_identical(x1, parse_dttm_fmt_(fmt, pattern = "H"))
  expect_identical(x2, parse_dttm_fmt_(fmt, pattern = "HH"))
  expect_identical(x3, parse_dttm_fmt_(fmt, pattern = "H+"))
})

test_that("`parse_dttm_fmt_`: only the first match is returned", {
  fmt <- "H M S H"

  x1 <-
    tibble::tibble(
      pat = "H",
      cap = "H",
      start = 1L,
      end = 1L,
      len = 1L
    )

  x2 <-
    tibble::tibble(
      pat = character(),
      cap = character(),
      start = integer(),
      end = integer(),
      len = integer()
    )

  x3 <-
    tibble::tibble(
      pat = "H+",
      cap = "H",
      start = 1L,
      end = 1L,
      len = 1L
    )

  expect_identical(x1, parse_dttm_fmt_(fmt, pattern = "H"))
  expect_identical(x2, parse_dttm_fmt_(fmt, pattern = "HH"))
  expect_identical(x3, parse_dttm_fmt_(fmt, pattern = "H+"))
})

test_that("`fmt_rg`: basic usage", {
  # Default
  observed <- fmt_rg()

  sec <- r"[(\b\d|\d{2})(\.\d*)?]"
  min <- r"[(\b\d|\d{2})]"
  hour <- r"[\d?\d]"
  mday <- r"[\b\d|\d{2}]"
  mon <- stringr::str_glue(r"[\d\d|{months_abb_regex()}]")
  year <- r"[(\d{2})?\d{2}]"
  sec_na <- ""
  min_na <- ""
  hour_na <- ""
  mday_na <- ""
  mon_na <- ""
  year_na <- ""

  expected <- c(
    sec = stringr::str_glue("(?<sec>{sec}{sec_na})"),
    min = stringr::str_glue("(?<min>{min}{min_na})"),
    hour = stringr::str_glue("(?<hour>{hour}{hour_na})"),
    mday = stringr::str_glue("(?<mday>{mday}{mday_na})"),
    mon = stringr::str_glue("(?<mon>{mon}{mon_na})"),
    year = stringr::str_glue("(?<year>{year}{year_na})")
  )

  expect_identical(observed, expected)

  # Pass an explicit regex for numerical months
  observed_m <- fmt_rg(mon = r"[\b\d|\d{2}]")

  mon <- r"[\b\d|\d{2}]"
  expected_m <- c(
    sec = stringr::str_glue("(?<sec>{sec}{sec_na})"),
    min = stringr::str_glue("(?<min>{min}{min_na})"),
    hour = stringr::str_glue("(?<hour>{hour}{hour_na})"),
    mday = stringr::str_glue("(?<mday>{mday}{mday_na})"),
    mon = stringr::str_glue("(?<mon>{mon}{mon_na})"),
    year = stringr::str_glue("(?<year>{year}{year_na})")
  )

  expect_identical(observed_m, expected_m)

  # Use `"UNK"` for the year component only
  observed_yr_unk <- fmt_rg(year_na = "UNK")

  mon <- stringr::str_glue(r"[\d\d|{months_abb_regex()}]")
  year_na <- regex_or("UNK", .open = TRUE)

  expected_yr_unk <- c(
    sec = stringr::str_glue("(?<sec>{sec}{sec_na})"),
    min = stringr::str_glue("(?<min>{min}{min_na})"),
    hour = stringr::str_glue("(?<hour>{hour}{hour_na})"),
    mday = stringr::str_glue("(?<mday>{mday}{mday_na})"),
    mon = stringr::str_glue("(?<mon>{mon}{mon_na})"),
    year = stringr::str_glue("(?<year>{year}{year_na})")
  )

  expect_identical(observed_yr_unk, expected_yr_unk)
  # Test if date/time components accept `"UNK"` as a possible pattern (useful
  # to match funny codes for `NA`).
  observed_unk <- fmt_rg(na = "UNK")
  sec_na <- regex_or("UNK", .open = TRUE)
  min_na <- regex_or("UNK", .open = TRUE)
  hour_na <- regex_or("UNK", .open = TRUE)
  mday_na <- regex_or("UNK", .open = TRUE)
  mon_na <- regex_or("UNK", .open = TRUE)
  year_na <- regex_or("UNK", .open = TRUE)

  expected_unk <- c(
    sec = stringr::str_glue("(?<sec>{sec}{sec_na})"),
    min = stringr::str_glue("(?<min>{min}{min_na})"),
    hour = stringr::str_glue("(?<hour>{hour}{hour_na})"),
    mday = stringr::str_glue("(?<mday>{mday}{mday_na})"),
    mon = stringr::str_glue("(?<mon>{mon}{mon_na})"),
    year = stringr::str_glue("(?<year>{year}{year_na})")
  )

  expect_identical(observed_unk, expected_unk)
})

test_that("`dttm_fmt_to_regex`: basic usage", {
  expect_identical(
    dttm_fmt_to_regex("y"),
    "^(?<year>(\\d{2})?\\d{2})$"
  )

  expect_identical(
    dttm_fmt_to_regex("y", anchored = FALSE),
    "(?<year>(\\d{2})?\\d{2})"
  )
  # nolint start
  expect_identical(
    dttm_fmt_to_regex("m"),
    "^(?<mon>\\d\\d|[Jj][Aa][Nn]|[Ff][Ee][Bb]|[Mm][Aa][Rr]|[Aa][Pp][Rr]|[Mm][Aa][Yy]|[Jj][Uu][Nn]|[Jj][Uu][Ll]|[Aa][Uu][Gg]|[Ss][Ee][Pp]|[Oo][Cc][Tt]|[Nn][Oo][Vv]|[Dd][Ee][Cc])$"
  )

  expect_identical(
    dttm_fmt_to_regex("ymd"),
    "^(?<year>(\\d{2})?\\d{2})(?<mon>\\d\\d|[Jj][Aa][Nn]|[Ff][Ee][Bb]|[Mm][Aa][Rr]|[Aa][Pp][Rr]|[Mm][Aa][Yy]|[Jj][Uu][Nn]|[Jj][Uu][Ll]|[Aa][Uu][Gg]|[Ss][Ee][Pp]|[Oo][Cc][Tt]|[Nn][Oo][Vv]|[Dd][Ee][Cc])(?<mday>\\b\\d|\\d{2})$"
  )

  expect_identical(
    dttm_fmt_to_regex("ymd HH:MM:SS"),
    "^(?<year>(\\d{2})?\\d{2})(?<mon>\\d\\d|[Jj][Aa][Nn]|[Ff][Ee][Bb]|[Mm][Aa][Rr]|[Aa][Pp][Rr]|[Mm][Aa][Yy]|[Jj][Uu][Nn]|[Jj][Uu][Ll]|[Aa][Uu][Gg]|[Ss][Ee][Pp]|[Oo][Cc][Tt]|[Nn][Oo][Vv]|[Dd][Ee][Cc])(?<mday>\\b\\d|\\d{2}) (?<hour>\\d?\\d):(?<min>(\\b\\d|\\d{2})):(?<sec>(\\b\\d|\\d{2})(\\.\\d*)?)$"
  )
  # nolint end
})

test_that("`regrex_or`: basic usage", {
  expect_identical(regex_or(c("jan", "feb")), "jan|feb")

  # Setting `.open` and/or `.close` to `TRUE` can be handy if this regex
  # is to be combined into a larger regex.
  expect_identical(
    paste0(regex_or(c("jan", "feb"), .close = TRUE), r"{\d{2}}"),
    "jan|feb|\\d{2}"
  )
})

test_that("`parse_dttm_fmt`: empty fmt", {
  expect_identical(fmt_dttmc(), parse_dttm_fmt("", pattern = "y"))
  expect_error(parse_dttm_fmt_(character(), pattern = "y"))
})

test_that("`parse_dttm_fmt` works as expected", {
  # ymd
  observed_ymd <- parse_dttm_fmt("ymd")
  expected_ymd <- tibble::tribble(
    ~fmt_c, ~pat, ~cap, ~start, ~end, ~len, ~ord,
    "year", "y+",  "y",     1L,   1L,   1L,   1L,
    "mon",  "m+",  "m",     2L,   2L,   1L,   2L,
    "mday", "d+",  "d",     3L,   3L,   1L,   3L
  )

  expect_identical(observed_ymd, expected_ymd)

  # hms
  observed_hms <- parse_dttm_fmt("H:M:S")
  expected_hms <- tibble::tribble(
    ~fmt_c, ~pat, ~cap, ~start, ~end, ~len, ~ord,
    "hour", "H+",  "H",     1L,   1L,   1L,   1L,
    NA,       NA,  ":",     2L,   2L,   1L,   NA,
    "min",  "M+",  "M",     3L,   3L,   1L,   2L,
    NA,       NA,  ":",     4L,   4L,   1L,   NA,
    "sec",  "S+",  "S",     5L,   5L,   1L,   3L
  )

  expect_identical(observed_hms, expected_hms)

  # ymdhms
  observed_ymdhms <- parse_dttm_fmt("ymd HMS")
  expected_ymdhms <- tibble::tribble(
    ~fmt_c, ~pat, ~cap, ~start, ~end, ~len, ~ord,
    "year", "y+",  "y",     1L,   1L,   1L,   1L,
    "mon",  "m+",  "m",     2L,   2L,   1L,   2L,
    "mday", "d+",  "d",     3L,   3L,   1L,   3L,
    NA,       NA,  " ",     4L,   4L,   1L,   NA,
    "hour", "H+",  "H",     5L,   5L,   1L,   4L,
    "min",  "M+",  "M",     6L,   6L,   1L,   5L,
    "sec",  "S+",  "S",     7L,   7L,   1L,   6L
  )

  expect_identical(observed_ymdhms, expected_ymdhms)

  # Repeating the same special patterns, e.g. "yy" still counts as one pattern
  # only.
  observed_ymdhms_1 <- parse_dttm_fmt("yymmdd HHMMSS")
  expected_ymdhms_1 <- tibble::tribble(
    ~fmt_c, ~pat, ~cap, ~start, ~end, ~len, ~ord,
    "year", "y+", "yy",     1L,   2L,   2L,   1L,
    "mon",  "m+", "mm",     3L,   4L,   2L,   2L,
    "mday", "d+", "dd",     5L,   6L,   2L,   3L,
    NA,       NA,  " ",     7L,   7L,   1L,   NA,
    "hour", "H+", "HH",     8L,   9L,   2L,   4L,
    "min",  "M+", "MM",    10L,  11L,   2L,   5L,
    "sec",  "S+", "SS",    12L,  13L,   2L,   6L
  )

  expect_identical(observed_ymdhms_1, expected_ymdhms_1)

  # `"y"`, `"m"`, `"d"`, `"H"`, `"M"` or `"S"` are reserved patterns
  # that are matched first and interpreted as format components. Example: the
  # first "y" in "year" is parsed as meaning year followed by "ear y". The
  # second "y" is not longer matched because a first match already succeed.
  observed_y <- parse_dttm_fmt("year y")
  expected_y <- tibble::tribble(
    ~fmt_c, ~pat,    ~cap, ~start, ~end, ~len, ~ord,
    "year", "y+",     "y",     1L,   1L,   1L,   1L,
    NA,       NA, "ear y",     2L,   6L,   5L,   NA
  )

  expect_identical(observed_y, expected_y)

  # Specify custom patterns
  observed_cus <- parse_dttm_fmt(
    "year month day",
    fmt_cmp(
      year = "year",
      mon = "month",
      mday = "day"
    )
  )

  expected_cus <- tibble::tribble(
    ~fmt_c,    ~pat,    ~cap, ~start, ~end, ~len, ~ord,
    "year",  "year",  "year",     1L,   4L,   4L,   1L,
    NA,          NA,     " ",     5L,   5L,   1L,   NA,
    "mon",  "month", "month",     6L,  10L,   5L,   2L,
    NA,          NA,     " ",    11L,  11L,   1L,   NA,
    "mday",   "day",   "day",    12L,  14L,   3L,   3L
  )

  expect_identical(observed_cus, expected_cus)
})
