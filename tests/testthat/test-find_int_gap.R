test_that("`find_int_gap()`: one interval", {
  tbl <- find_int_gap(c(1L:3L, 7L:10L))

  expect_identical(tbl$start, 4L)
  expect_identical(tbl$end, 6L)
})

test_that("`find_int_gap()`: two intervals", {
  tbl <- find_int_gap(c(1L:3L, 7L:10L, 15L:20L))

  expect_identical(tbl$start, c(4L, 11L))
  expect_identical(tbl$end, c(6L, 14L))
})

test_that("`find_int_gap()`: explicit endpoints", {
  tbl <- find_int_gap(c(3L:5L, 8L), xmin = 0L, xmax = 10L)

  expect_identical(tbl$start, c(0L, 6L, 9L))
  expect_identical(tbl$end, c(2L, 7L, 10L))
})

test_that("`find_int_gap()`: no intervals", {
  tbl <- find_int_gap(0L:5L)
  expect_identical(tbl, tibble::tibble(start = integer(), end = integer()))
})

test_that("`find_int_gap()`: ensure `x` is integerish", {
  expect_error(find_int_gap(c(1.5, pi)))
})

test_that("`find_int_gap()`: ensure `xmin` and `xmax` are integer scalars", {
  # Error because `xmin` and `xmax` are vectors
  expect_error(find_int_gap(c(1L:3L, 7L:10L), xmin = 1L:2L))
  expect_error(find_int_gap(c(1L:3L, 7L:10L), xmax = 3L:4L))

  # Error because `xmin` and `xmax` are double
  expect_error(find_int_gap(c(1L:3L, 7L:10L), xmin = 1.5))
  expect_error(find_int_gap(c(1L:3L, 7L:10L), xmax = 1.5))

  # Error because `xmin` and `xmax` are character
  expect_error(find_int_gap(c(1L:3L, 7L:10L), xmin = "1"))
  expect_error(find_int_gap(c(1L:3L, 7L:10L), xmax = "2"))
})
