test_that("`yy_to_yyyy()`: basic usage", {
  # Default cutoff is at `68`.
  x1 <- c(0L, 1L, 50L, 68L, 69L, 70L)
  y1 <- c(2000L, 2001L, 2050L, 2068L, 1969L, 1970L)
  expect_identical(yy_to_yyyy(x1), y1)

  # Different cutoff, e.g. `79`.
  x2 <- 75L:85L
  y2 <-
    c(
      2075L,
      2076L,
      2077L,
      2078L,
      2079L,
      1980L,
      1981L,
      1982L,
      1983L,
      1984L,
      1985L
    )
  expect_identical(yy_to_yyyy(x2, cutoff_2000 = 79L), y2)

  # Four-digit years remain altered.
  x3 <- 1965L:1975L
  expect_identical(yy_to_yyyy(x3), x3)
})
