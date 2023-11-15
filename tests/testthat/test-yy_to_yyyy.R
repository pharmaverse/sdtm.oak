test_that("`yy_to_yyyy()`: basic usage", {

  # Default cutoff is at `68`.
  x1 <- c(0, 1, 50, 68, 69, 70)
  y1 <- c(2000, 2001, 2050, 2068, 1969, 1970)
  expect_equal(yy_to_yyyy(x1), y1)

  # Different cutoff, e.g. `79`.
  x2 <- 75:85
  y2 <-
    c(2075L,
      2076L,
      2077L,
      2078L,
      2079L,
      1980L,
      1981L,
      1982L,
      1983L,
      1984L,
      1985L)
  expect_equal(yy_to_yyyy(x2, cutoff_2000 = 79L), y2)

  # Four-digit years remain altered.
  x3 <- 1965:1975
  expect_equal(yy_to_yyyy(x3), x3)
})
