test_that("`format_iso8601()`: basic usage", {
  cols <- c("year", "mon", "mday", "hour", "min", "sec")
  m <- matrix(
    c(
      "99", "00", "01",
      "Jan", "feb", "03",
      "1", "01", "31",
      "00", "12", "23",
      "00", "59", "10",
      "42", "5.15", NA
    ),
    ncol = 6L,
    dimnames = list(c(), cols)
  )

  expect_identical(
    format_iso8601(m),
    c(
      "1999-01-01T00:00:42",
      "2000-02-01T12:59:05.15",
      "2001-03-31T23:10"
    )
  )
})
