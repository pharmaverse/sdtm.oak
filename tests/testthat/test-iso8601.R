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
  expect_identical(zero_pad_whole_number(c(-1L, 0L, 1L, 10L, 99L, 100L), n = 2L),
                   c(NA, "00", "01", "10", "99", NA))
  expect_identical(zero_pad_whole_number(c(-1L, 0L, 1L, 10L, 99L, 100L), n = 3L),
                   c(NA, "000", "001", "010", "099", "100"))
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
