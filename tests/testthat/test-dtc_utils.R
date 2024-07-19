test_that("`assert_dtc_fmt()`: basic usage", {
  fmt <- c("ymd", "y m d", "dmy", "HM", "H:M:S", "y-m-d H:M:S")
  expect_identical(
    assert_dtc_fmt(fmt),
    fmt
  )

  expect_error(assert_dtc_fmt("y years m months d days"))
})

test_that("`assert_dtc_format()`: basic usage", {
  expect_identical(assert_dtc_format("ymd"), "ymd")
  expect_identical(assert_dtc_format(c("ymd", "y-m-d")), c("ymd", "y-m-d"))
  expect_identical(
    assert_dtc_format(list(c("ymd", "y-m-d"), "H:M:S")),
    list(c("ymd", "y-m-d"), "H:M:S")
  )

  expect_error(assert_dtc_format("year, month, day"))
})

test_that("`assert_capture_matrix()`: basic usage", {
  cols <- c("year", "mon", "mday", "hour", "min", "sec")
  m <- matrix(NA_character_, nrow = 1L, ncol = 6L, dimnames = list(NULL, cols))
  expect_identical(assert_capture_matrix(m), m)

  expect_error(assert_capture_matrix(character()))
  expect_error(assert_capture_matrix(matrix(data = NA_character_, nrow = 0L, ncol = 0L)))
  expect_error(assert_capture_matrix(matrix(data = NA_character_, nrow = 1L)))
})

test_that("`complete_capture_matrix()`: basic usage", {
  # Input with no cols and rows
  input <- matrix(data = NA_character_, nrow = 0L, ncol = 0L)
  expected_output <- matrix(
    data = NA_character_,
    nrow = 0L,
    ncol = 6L,
    dimnames = list(NULL, c("year", "mon", "mday", "hour", "min", "sec"))
  )

  expect_identical(
    complete_capture_matrix(input),
    expected_output
  )

  # Input with no cols and 1 row
  input <- matrix(data = NA_character_, nrow = 1L)
  expected_output <- matrix(
    data = NA_character_,
    nrow = 1L,
    ncol = 6L,
    dimnames = list(NULL, c("year", "mon", "mday", "hour", "min", "sec"))
  )

  expect_identical(
    complete_capture_matrix(input),
    expected_output
  )

  # Input with incomplete cols
  input <-
    matrix(
      NA_character_,
      nrow = 1L,
      ncol = 2L,
      dimnames = list(NULL, c("year", "sec"))
    )

  expected_output <- matrix(
    data = NA_character_,
    nrow = 1L,
    ncol = 6L,
    dimnames = list(NULL, c("year", "mon", "mday", "hour", "min", "sec"))
  )

  expect_identical(
    complete_capture_matrix(input),
    expected_output
  )

  # Input with year and second specified
  input <-
    matrix(
      c("2020", "10"),
      nrow = 1L,
      ncol = 2L,
      dimnames = list(NULL, c("year", "sec"))
    )

  expected_output <-
    matrix(
      data = c("2020", rep(NA, 4L), "10"),
      nrow = 1L,
      ncol = 6L,
      dimnames = list(NULL, c("year", "mon", "mday", "hour", "min", "sec"))
    )

  expect_identical(
    complete_capture_matrix(input),
    expected_output
  )

  # Any other existing columns are dropped.
  input <-
    matrix(
      c("2020", "10"),
      nrow = 1L,
      ncol = 2L,
      dimnames = list(NULL, c("semester", "quarter"))
    )

  expected_output <- matrix(
    data = NA_character_,
    nrow = 1L,
    ncol = 6L,
    dimnames = list(NULL, c("year", "mon", "mday", "hour", "min", "sec"))
  )

  expect_identical(
    complete_capture_matrix(input),
    expected_output
  )
})

test_that("`coalesce_capture_matrices()`: basic usage", {
  cols <- c("year", "mon", "mday", "hour", "min", "sec")
  dates <- c("2020", "01", "01", "20", NA, NA)
  times <- c(NA, NA, NA, "10", "00", "05")
  m_dates <- matrix(dates, nrow = 1L, ncol = 6L, dimnames = list(NULL, cols))
  m_times <- matrix(times, nrow = 1L, ncol = 6L, dimnames = list(NULL, cols))

  # Note how the hour "20" takes precedence over "10"
  expected_output <- tibble::tribble(
    ~year,  ~mon, ~mday, ~hour, ~min, ~sec,
    "2020", "01",  "01",  "20", "00", "05"
  ) |>
    as.matrix()

  expect_identical(coalesce_capture_matrices(m_dates, m_times), expected_output)

  # Reverse the order of the inputs and now hour "10" takes precedence
  expected_output <- tibble::tribble(
    ~year,  ~mon, ~mday, ~hour, ~min, ~sec,
    "2020", "01",  "01",  "10", "00", "05"
  ) |>
    as.matrix()

  expect_identical(coalesce_capture_matrices(m_times, m_dates), expected_output)

  # Single inputs should result in the same output as the input
  expected_output <- tibble::tribble(
    ~year,  ~mon, ~mday, ~hour, ~min, ~sec,
    "2020", "01",  "01",  "20",   NA,   NA
  ) |>
    as.matrix()

  expect_identical(coalesce_capture_matrices(m_dates), expected_output)

  expected_output <- tibble::tribble(
    ~year,  ~mon, ~mday, ~hour, ~min, ~sec,
    NA,       NA,    NA,  "10", "00", "05"
  ) |>
    as.matrix()

  expect_identical(coalesce_capture_matrices(m_times), expected_output)
})
