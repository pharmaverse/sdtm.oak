test_that("Warn if date variable parameter is NULL", {
  EX <- tibble::tribble(
    ~patient_number,    ~EX_ST_DT,
    "001",           "26-04-2022"
  )

  warning_msg <- "Date variable NA or Time variable NA not present in source data"
  expect_warning(
    observed <- cal_min_max_date(EX,
      date_variable = NA,
      time_variable = NA,
      val_type = "max",
      date_format = "dd-mmm-yyyy",
      time_format = "H:M"
    ),
    regexp = warning_msg
  )

  expected <- stats::setNames(
    data.frame(matrix(ncol = 2L, nrow = 0L)),
    c("patient_number", "datetime")
  )

  expect_identical(observed, expected)
})

test_that("cal_min_max_date works as expected", {
  EX <- tibble::tribble(
    ~patient_number,    ~EX_ST_DT, ~EX_ST_TM,
    "001",           "26-04-2022", "10:20",
    "001",           "25-04-2022", "10:15",
    "001",           "25-04-2022", "10:19",
    "002",           "26-05-2022", "06:23",
    "002",           "26-05-2022", "04:59",
    "002",           "26-05-2022", "05:59"
  )

  expected_min <- tibble::tribble(
    ~patient_number,          ~datetime,
    "001",           "2022-04-25T10:15",
    "002",           "2022-05-26T04:59"
  )

  expected_max <- tibble::tribble(
    ~patient_number,           ~datetime,
    "002",            "2022-05-26T06:23",
    "001",            "2022-04-26T10:20"
  )

  observed_min <- cal_min_max_date(EX,
    date_variable = "EX_ST_DT",
    time_variable = "EX_ST_TM",
    val_type = "min",
    date_format = "dd-mmm-yyyy",
    time_format = "H:M"
  )

  expect_identical(observed_min, expected_min)

  observed_max <- cal_min_max_date(EX,
    date_variable = "EX_ST_DT",
    time_variable = "EX_ST_TM",
    val_type = "max",
    date_format = "dd-mmm-yyyy",
    time_format = "H:M"
  )

  expect_identical(observed_max, expected_max)
})

test_that("Warning is displayed if date or time variables parameters passed are not present", {
  EX <- tibble::tribble(
    ~patient_number,    ~EX_ST_DT,
    "001",           "26-04-2022"
  )

  warning_msg <- "Date variable EX_ST_DT or Time variable EX_ST_TM not present in source data"
  expect_warning(
    observed <- cal_min_max_date(EX,
      date_variable = "EX_ST_DT",
      time_variable = "EX_ST_TM",
      val_type = "max",
      date_format = "dd-mmm-yyyy",
      time_format = "H:M"
    ),
    regexp = warning_msg
  )

  expected <- stats::setNames(
    data.frame(matrix(ncol = 2L, nrow = 0L)),
    c("patient_number", "datetime")
  )

  expect_identical(observed, expected)
})
