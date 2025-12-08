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

test_that("cal_min_max_date works with date and time strings", {
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

test_that("cal_min_max_date works with Date and hms (time) objects", {
  EX <- tibble::tribble(
    ~patient_number,    ~EX_ST_DT, ~EX_ST_TM,
    "001",           as.Date("2022-04-26"), hms::hms(seconds = NULL, minutes = 20, hours = 10),
    "001",           as.Date("2022-04-25"), hms::hms(seconds = NULL, minutes = 15, hours = 10),
    "001",           as.Date("2022-04-25"), hms::hms(seconds = NULL, minutes = 19, hours = 10),
    "002",           as.Date("2022-05-26"), hms::hms(seconds = NULL, minutes = 23, hours = 6),
    "002",           as.Date("2022-05-26"), hms::hms(seconds = NULL, minutes = 59, hours = 4),
    "002",           as.Date("2022-05-26"), hms::hms(seconds = NULL, minutes = 59, hours = 5)
  )
  expected_min <- tibble::tribble(
    ~patient_number,          ~datetime,
    "001",           "2022-04-25T10:15:00",
    "002",           "2022-05-26T04:59:00"
  )

  expected_max <- tibble::tribble(
    ~patient_number,           ~datetime,
    "002",            "2022-05-26T06:23:00",
    "001",            "2022-04-26T10:20:00"
  )

  observed_min <- cal_min_max_date(EX,
    date_variable = "EX_ST_DT",
    time_variable = "EX_ST_TM",
    val_type = "min",
    date_format = "y-m-d",
    time_format = "H:M:S"
  )

  expect_identical(observed_min, expected_min)

  observed_max <- cal_min_max_date(EX,
    date_variable = "EX_ST_DT",
    time_variable = "EX_ST_TM",
    val_type = "max",
    date_format = "y-m-d",
    time_format = "H:M:S"
  )

  expect_identical(observed_max, expected_max)
})

test_that("cal_min_max_date works with Date objects (no time)", {
  
  EX <- tibble::tribble(
    ~patient_number,    ~EX_ST_DT,
    "001",           as.Date("04-25-22", "%m-%d-%y"),
    "001",           as.Date("04-25-22", "%m-%d-%y"),
    "002",           as.Date("05-26-22", "%m-%d-%y")
  )
  expected_min <- tibble::tribble(
    ~patient_number,          ~datetime,
    "001",           create_iso8601("04-25-22", .format = "m-d-y"),
    "002",           create_iso8601("05-26-22", .format = "m-d-y")
  )

  observed_min <- cal_min_max_date(EX,
    date_variable = "EX_ST_DT",
    time_variable = NA,
    val_type = "min",
    date_format = "y-m-d",
    time_format = NA
  )

  expect_identical(observed_min, expected_min)
})

