test_that("`iso8601_na()`: basic usage", {
  expect_equal(iso8601_na(c("10", "15")), c("10", "15"))
  expect_equal(iso8601_na(c("10", NA_character_)), c("10", "-"))
  expect_equal(iso8601_na(character()), character(0))
})

test_that("`iso8601_na()`: input can't be `NULL`", {
  expect_error(iso8601_na(NULL))
  expect_error(iso8601_na(c()))
})

test_that("`zero_pad_whole_number()`: ensure `x` is integerish", {
  expect_error(zero_pad_whole_number(pi))
  expect_error(zero_pad_whole_number("42"))
  expect_error(zero_pad_whole_number(sqrt(2)))
  expect_error(zero_pad_whole_number(TRUE))

  expect_no_error(zero_pad_whole_number(1))
  expect_no_error(zero_pad_whole_number(1.00))
  expect_no_error(zero_pad_whole_number(c(1:3)))
})

test_that("`zero_pad_whole_number()`: basic usage", {
  expect_equal(zero_pad_whole_number(c(-1, 0, 1)), c(NA, "00", "01"))
  expect_equal(zero_pad_whole_number(c(-1, 0, 1, 10, 99, 100), n = 2),
               c(NA, "00", "01", "10", "99", NA))
  expect_equal(zero_pad_whole_number(c(-1, 0, 1, 10, 99, 100), n = 3),
               c(NA, "000", "001", "010", "099", "100"))
})

test_that("`zero_pad_whole_number()`: ensure `n` is scalar integer", {
  expect_no_error(zero_pad_whole_number(1, n = 1))
  expect_error(zero_pad_whole_number(1, n = 1:2))
})

test_that("`iso8601_two_digits()`: basic usage", {
  x <- c("0", "00", "1", "01", "42", "100", NA_character_, "1.")
  y <- c("00", "00", "01", "01", "42", NA, NA, NA)
  expect_equal(iso8601_two_digits(x), y)
})
