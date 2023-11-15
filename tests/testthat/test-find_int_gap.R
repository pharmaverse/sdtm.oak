test_that("`find_int_gap()`: one interval", {

  tbl <- find_int_gap(c(1:3, 7:10))

  expect_equal(tbl$start, 4)
  expect_equal(tbl$end, 6)

})

test_that("`find_int_gap()`: two intervals", {

  tbl <- find_int_gap(c(1:3, 7:10, 15:20))

  expect_equal(tbl$start, c(4, 11))
  expect_equal(tbl$end, c(6, 14))

})

test_that("`find_int_gap()`: explicit endpoints", {

  tbl <- find_int_gap(c(3:5, 8), xmin = 0, xmax = 10)

  expect_equal(tbl$start, c(0, 6, 9))
  expect_equal(tbl$end, c(2, 7, 10))

})

test_that("`find_int_gap()`: no intervals", {

  tbl <- find_int_gap(0:5)
  expect_equal(tbl, tibble::tibble(start = integer(), end = integer()))

})

test_that("`find_int_gap()`: ensure `x` is integerish", {

  expect_error(find_int_gap(c(1.5, pi)))

})

test_that("`find_int_gap()`: ensure `xmin` and `xmax` are integer scalars", {

  # Error because `xmin` and `xmax` are vectors
  expect_error(find_int_gap(c(1:3, 7:10), xmin = 1:2))
  expect_error(find_int_gap(c(1:3, 7:10), xmax = 3:4))

  # Error because `xmin` and `xmax` are double
  expect_error(find_int_gap(c(1:3, 7:10), xmin = 1.5))
  expect_error(find_int_gap(c(1:3, 7:10), xmax = 1.5))

  # Error because `xmin` and `xmax` are character
  expect_error(find_int_gap(c(1:3, 7:10), xmin = "1"))
  expect_error(find_int_gap(c(1:3, 7:10), xmax = "2"))

})
