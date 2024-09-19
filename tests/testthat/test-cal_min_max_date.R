test_that("cal_min_max_date works as expected", {

  EX <- tibble::tribble(
    ~patient_number,    ~EX_ST_DT,  ~EX_ST_TM,
              "001", "26-04-2022",    "10:20",
              "001", "25-04-2022",    "10:15",
              "001", "25-04-2022",    "10:19",
              "002", "26-05-2022",    "06:23",
              "002", "26-05-2022",    "04:59",
              "002", "26-05-2022",    "05:59"
    )

  expected_min <- tibble::tribble(
    ~patient_number,          ~datetime,
              "001", "2022-04-25T10:15",
              "002", "2022-05-26T04:59"
    )

  expected_max <- tibble::tribble(
    ~patient_number,          ~datetime,
              "002", "2022-05-26T06:23",
              "001", "2022-04-26T10:20"
    )

  observed_min <- cal_min_max_date(EX,
                                   "EX_ST_DT",
                                   "EX_ST_TM",
                                   val_type = "min",
                                   date_format = "dd-mmm-yyyy",
                                   time_format = "H:M"
                                   )

  expect_identical(observed_min, expected_min)

  observed_max <- cal_min_max_date(EX,
                                   "EX_ST_DT",
                                   "EX_ST_TM",
                                   val_type = "max",
                                   date_format = "dd-mmm-yyyy",
                                   time_format = "H:M"
                                   )

  expect_identical(observed_max, expected_max)

})


