test_that("`add_problems()`: basic usage", {
  date <- c("2000-01-05", "", "1980-06-18", "1979-09-07")
  time <- c("001221", "22:35:05", "03:00:15", "07:09:00")
  dtc <- list(date, time)
  dttm <- c("2000-01-05", "T22:35:05", "1980-06-18T03:00:15", "1979-09-07T07:09:00")
  is_problem <- c(TRUE, TRUE, FALSE, FALSE)

  dttm2 <- add_problems(dttm, is_problem, dtc)
  dttm2_expected <- c("2000-01-05", "T22:35:05", "1980-06-18T03:00:15", "1979-09-07T07:09:00")

  expect_identical(as.vector(dttm2), dttm2_expected)
})

test_that("`any_problems()`: basic usage", {
  expect_false(any_problems(list(parse_dttm("1980-06-18", "y-m-d"))))

  expect_true(any_problems(list(parse_dttm("1980-06-18", "ymd"))))

  # Multiple records
  date <- c("2000-01-05", "2001/12/25", "1980-06-18", "1979-09-07")
  time <- c("00h12m21", "22:35:05", "03:00:15", "07:09:00")

  cap_matrix_date <- parse_dttm(date, "y-m-d")
  cap_matrix_time <- parse_dttm(time, "H:M:S")

  cap_matrices <- list(cap_matrix_date, cap_matrix_time)
  expect_identical(
    any_problems(cap_matrices),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})
