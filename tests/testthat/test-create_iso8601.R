test_that("`create_iso8601()`: individual date components", {
  x <- c("0", "50", "1950", "80", "1980", "2000")
  y0 <- create_iso8601(x, .format = "y", .check_format = FALSE)
  y1 <- c(NA, "2050", "1950", "1980", "1980", "2000")
  expect_identical(as.character(y0), y1)

  x <- c("0", "jan", "JAN", "JaN", "1", "01")
  y0 <- create_iso8601(x, .format = "m", .check_format = FALSE)
  y1 <- c(NA, "--01", "--01", "--01", NA, "--01")
  expect_identical(as.character(y0), y1)

  x <- c("0", "00", "1", "01", "10", "31")
  y0 <- create_iso8601(x, .format = "d", .check_format = FALSE)
  y1 <- c("----00", "----00", "----01", "----01", "----10", "----31")
  expect_identical(as.character(y0), y1)
})

test_that("`create_iso8601()`: dates", {
  y1 <- c("1999-01-01", "2000-01-01", "1999-01-01", "1999-12-31")

  x <- c("19990101", "20000101", "990101", "991231")
  y0 <- create_iso8601(x, .format = "ymd", .check_format = FALSE)
  expect_identical(as.character(y0), y1)

  x <- c("1999-01-01", "2000-01-01", "99-01-01", "99-12-31")
  y0 <- create_iso8601(x, .format = "y-m-d", .check_format = FALSE)
  expect_identical(as.character(y0), y1)

  x <- c("1999 01 01", "2000 01 01", "99 01 01", "99 12 31")
  y0 <- create_iso8601(x, .format = "y m d", .check_format = FALSE)
  expect_identical(as.character(y0), y1)
})

test_that("`create_iso8601()`: times: hours and minutes", {
  y1 <- c("-----T15:20", "-----T00:10", "-----T23:01", "-----T00:00")

  x <- c("1520", "0010", "2301", "0000")
  y0 <- create_iso8601(x, .format = "HM", .check_format = FALSE)
  expect_identical(as.character(y0), y1)

  x <- c("15:20", "00:10", "23:01", "00:00")
  y0 <- create_iso8601(x, .format = "H:M", .check_format = FALSE)
  expect_identical(as.character(y0), y1)

  x <- c("15h20", "00h10", "23h01", "00h00")
  y0 <- create_iso8601(x, .format = "HhM", .check_format = FALSE)
  expect_identical(as.character(y0), y1)
})

test_that("`create_iso8601()`: times: hours, minutes and seconds", {
  x <- c("152000", "001059", "230112.123", "00002.")
  y0 <- create_iso8601(x, .format = "HMS", .check_format = FALSE)
  y1 <- c("-----T15:20:00", "-----T00:10:59", "-----T23:01:12.123", "-----T00:00:02")
  expect_identical(as.character(y0), y1)

  x <- c("15:20:00", "00:10:59", "23:01:12.123", "00:00:2.", "5:1:4")
  y0 <- create_iso8601(x, .format = "H:M:S", .check_format = FALSE)
  y1 <- c(y1, "-----T05:01:04")
  expect_identical(as.character(y0), y1)
})


test_that("`create_iso8601()`: dates and times", {
  dates <- c("1999-01-01", "2000-01-01", "99-01-01", "99-12-31")
  times <- c("1520", "0010", "2301", "0000")
  iso8601_dttm <- create_iso8601(dates, times, .format = c("y-m-d", "HM"), .check_format = FALSE)
  expectation <-
    c(
      "1999-01-01T15:20",
      "2000-01-01T00:10",
      "1999-01-01T23:01",
      "1999-12-31T00:00"
    )
  expect_identical(as.character(iso8601_dttm), expectation)
})

# https://github.com/pharmaverse/sdtm.oak/pull/33#discussion_r1436195327
test_that("`create_iso8601()`: expect problems", {
  dates <- c("999999999", "2000-01-01", "99-01-01", "99-12-31")
  times <- c("1520", "0010", "2301", "999999999999")
  iso8601_dttm <- create_iso8601(dates, times, .format = c("y-m-d", "HM"), .check_format = FALSE)
  expectation <-
    structure(
      c(
        "-----T15:20",
        "2000-01-01T00:10",
        "1999-01-01T23:01",
        "1999-12-31"
      ),
      problems = structure(
        list(
          ..i = c(1L, 4L),
          ..var1 = c(
            "999999999",
            "99-12-31"
          ),
          ..var2 = c("1520", "999999999999")
        ),
        row.names = c(
          NA,
          -2L
        ),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      class = "iso8601"
    )
  expect_identical(iso8601_dttm, expectation)
})
