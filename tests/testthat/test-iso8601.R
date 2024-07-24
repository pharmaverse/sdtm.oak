test_that("`iso8601_na()`: basic usage", {
  expect_identical(iso8601_na(c("10", "15")), c("10", "15"))
  expect_identical(iso8601_na(c("10", NA_character_)), c("10", "-"))
  expect_identical(iso8601_na(character()), character(0L))
})

test_that("`iso8601_na()`: input can't be `NULL`", {
  expect_error(iso8601_na(NULL))
  expect_error(iso8601_na(c()))
})

test_that("`zero_pad_whole_number()`: ensure `x` is integerish", {
  expect_error(zero_pad_whole_number(pi))
  expect_error(zero_pad_whole_number("42"))
  expect_error(zero_pad_whole_number(sqrt(2.0)))
  expect_error(zero_pad_whole_number(TRUE))

  expect_no_error(zero_pad_whole_number(1L))
  expect_no_error(zero_pad_whole_number(1.00))
  expect_no_error(zero_pad_whole_number(c(1L:3L)))
})

test_that("`zero_pad_whole_number()`: basic usage", {
  expect_identical(zero_pad_whole_number(c(-1L, 0L, 1L)), c(NA, "00", "01"))
  expect_identical(
    zero_pad_whole_number(c(-1L, 0L, 1L, 10L, 99L, 100L), n = 2L),
    c(NA, "00", "01", "10", "99", NA)
  )
  expect_identical(
    zero_pad_whole_number(c(-1L, 0L, 1L, 10L, 99L, 100L), n = 3L),
    c(NA, "000", "001", "010", "099", "100")
  )
})

test_that("`zero_pad_whole_number()`: ensure `n` is scalar integer", {
  expect_no_error(zero_pad_whole_number(1L, n = 1L))
  expect_error(zero_pad_whole_number(1L, n = 1L:2L))
})

test_that("`iso8601_two_digits()`: basic usage", {
  x <- c("0", "00", "1", "01", "42", "100", NA_character_, "1.")
  y <- c("00", "00", "01", "01", "42", NA, NA, NA)
  expect_identical(iso8601_two_digits(x), y)
})

test_that("`iso8601_year()`: basic usage", {
  expect_identical(
    iso8601_year(c("0", "1", "2", "50", "68", "69", "90", "99", "00")),
    c("2000", "2001", "2002", "2050", "2068", "1969", "1990", "1999", "2000")
  )

  # By default, `cutoff_2000` is at 68.
  expect_identical(
    iso8601_year(c("67", "68", "69", "70")),
    c("2067", "2068", "1969", "1970")
  )

  expect_identical(
    iso8601_year(c("1967", "1968", "1969", "1970")),
    c("1967", "1968", "1969", "1970")
  )

  # Set cutoff_2000 to something else
  expect_identical(
    iso8601_year(as.character(0L:50L), cutoff_2000 = 25L),
    as.character(c(2000L:2025L, 1926L:1950L))
  )

  expect_identical(
    iso8601_year(as.character(1900L:1950L), cutoff_2000 = 25L),
    as.character(c(1900L:1950L))
  )
})

test_that("`iso8601_mon()`: basic usage", {
  expect_identical(
    iso8601_mon(c(NA, "0", "1", "2", "10", "11", "12")),
    c(NA, "00", "01", "02", "10", "11", "12")
  )

  # No semantic validation is performed on the numeric months, so `"13"` stays
  # `"13"` but representations that can't be represented as two-digit numbers
  # become `NA`.
  expect_identical(
    iso8601_mon(c("13", "99", "100", "-1")),
    c("13", "99", NA, NA)
  )

  mon <- month.abb
  expect_identical(
    iso8601_mon(mon),
    c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  )
})

test_that("`iso8601_sec()`: basic usage", {
  expect_identical(
    iso8601_sec(c(NA, "0", "1", "10", "59", "99", "100")),
    c(NA, "00", "01", "10", "59", "99", NA)
  )
})

test_that("`iso8601_truncate()`: basic usage", {
  x <-
    c(
      "1999-01-01T15:20:01",
      "1999-01-01T15:20:-",
      "1999-01-01T15:-:-",
      "1999-01-01T-:-:-",
      "1999-01--T-:-:-",
      "1999----T-:-:-",
      "-----T-:-:-"
    )

  expect_identical(
    iso8601_truncate(x),
    c(
      "1999-01-01T15:20:01", "1999-01-01T15:20", "1999-01-01T15", "1999-01-01",
      "1999-01", "1999", NA
    )
  )

  # With `empty_as_na = FALSE` empty strings are not replaced with `NA`
  expect_true(is.na(iso8601_truncate("-----T-:-:-", empty_as_na = TRUE)))
  expect_identical(iso8601_truncate("-----T-:-:-", empty_as_na = FALSE), "")

  # Truncation only happens if missing components are the right most end,
  # otherwise they remain unaltered.
  expect_identical(
    iso8601_truncate(
      c(
        "1999----T15:20:01",
        "1999-01-01T-:20:01",
        "1999-01-01T-:-:01",
        "1999-01-01T-:-:-"
      )
    ),
    c(
      "1999----T15:20:01",
      "1999-01-01T-:20:01",
      "1999-01-01T-:-:01",
      "1999-01-01"
    )
  )
})
