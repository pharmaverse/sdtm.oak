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

test_that("`parse_dttm_fmt_`: patterns", {

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

test_that("`parse_dttm_fmt`: empty fmt", {

  x <-
    tibble::tibble(
      fmt_c = character(),
      pat = character(),
      cap = character(),
      start = integer(),
      end = integer(),
      len = integer(),
      ord = integer()
    )
  expect_identical(x, parse_dttm_fmt("", pattern = "y"))
  expect_error(parse_dttm_fmt_(character(), pattern = "y"))
})
