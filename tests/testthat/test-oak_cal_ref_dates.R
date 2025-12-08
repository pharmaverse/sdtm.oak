dm <- tibble::tribble(
  ~patient_number,   ~USUBJID, ~SUBJID, ~SEX,
  "001",           "XXXX-001",   "001",  "F",
  "002",           "XXXX-002",   "002",  "M",
  "003",           "XXXX-003",   "003",  "M"
)

expected <- tibble::tribble(
  ~patient_number,    ~USUBJID, ~SUBJID, ~SEX,           ~RFSTDTC,           ~RFENDTC,
  "001",            "XXXX-001",   "001",  "F", "2023-05-15T08:19",       "2023-06-11",
  "002",            "XXXX-002",   "002",  "M",       "2023-07-25", "2023-11-03T20:30",
  "003",            "XXXX-003",   "003",  "M",      NA_character_,      NA_character_
)

ref_date_conf_df <- tibble::tribble(
  ~raw_dataset_name, ~date_var, ~time_var, ~dformat, ~tformat, ~sdtm_var_name,
  "EX1", "EX_ST_DT1", "EX_ST_TM1", "dd-mm-yyyy", "H:M", "RFSTDTC",
  "EX2", "EX_ST_DT2", NA_character_, "dd-mmm-yyyy", NA_character_, "RFSTDTC",
  "EX1", "EX_EN_DT1", "EX_EN_TM1", "dd-mm-yyyy", "H:M", "RFENDTC",
  "EX2", "EX_ST_DT2", NA_character_, "dd-mmm-yyyy", NA_character_, "RFENDTC"
)

EX1 <- tibble::tribble(
  ~patient_number,   ~EX_ST_DT1,   ~EX_EN_DT1, ~EX_ST_TM1,    ~EX_EN_TM1,
  "001",           "15-05-2023", "15-05-2023",    "10:20",       "11:00",
  "001",           "15-05-2023", "15-05-2023",     "9:15",       "10:00",
  "001",           "15-05-2023", "15-05-2023",     "8:19",       "09:00",
  "002",           "02-10-2023", "02-10-2023",  "UNK:UNK", NA_character_,
  "002",           "0l-11-2023", "03-11-2023",    "11:19",       "20:30"
)

EX2 <- tibble::tribble(
  ~patient_number,    ~EX_ST_DT2,
  "001",           "11-JUN-2023",
  "002",           "24-OCT-2023",
  "002",           "25-JUL-2023",
  "002",           "30-OCT-2023",
  "002",          "UNK-OCT-2023"
)

raw_source <- list(EX1 = EX1, EX2 = EX2)

test_that("Calculate the Reference dates :RFSTDTC", {
  observed_rfstdtc <- oak_cal_ref_dates(dm,
    der_var = "RFSTDTC",
    min_max = "min",
    ref_date_config_df = ref_date_conf_df,
    raw_source
  )
  expected_rfstdtc <- expected |> dplyr::select(-"RFENDTC")

  expect_identical(observed_rfstdtc, expected_rfstdtc)
})

test_that("Calculate the Reference dates :RFENDTC", {
  observed_rfendtc <- oak_cal_ref_dates(dm,
    der_var = "RFENDTC",
    min_max = "max",
    ref_date_config_df = ref_date_conf_df,
    raw_source
  )

  expected_rfendtc <- expected |> dplyr::select(-"RFSTDTC")
  expect_identical(observed_rfendtc, expected_rfendtc)
})

test_that("oak_cal_ref_dates works with Date and hms (time) objects", {
  dm <- tibble::tribble(
    ~patient_number,
    "001",
    "002"
  )

  EX <- tibble::tribble(
    ~patient_number, ~EX_ST_DT,             ~EX_ST_TM,
    "001",           as.Date("2022-04-26"), hms::hms(seconds = NULL, minutes = 20L, hours = 10L),
    "001",           as.Date("2022-04-25"), hms::hms(seconds = NULL, minutes = 15L, hours = 10L),
    "001",           as.Date("2022-04-25"), hms::hms(seconds = NULL, minutes = 19L, hours = 10L),
    "002",           as.Date("2022-05-26"), hms::hms(seconds = NULL, minutes = 23L, hours = 6L),
    "002",           as.Date("2022-05-26"), hms::hms(seconds = NULL, minutes = 59L, hours = 4L),
    "002",           as.Date("2022-05-26"), hms::hms(seconds = NULL, minutes = 59L, hours = 5L)
  )

  ref_date_conf_df <- tibble::tribble(
    ~raw_dataset_name, ~date_var, ~time_var, ~dformat, ~tformat, ~sdtm_var_name,
    "EX", "EX_ST_DT", "EX_ST_TM", "y-m-d", "H:M:S", "RFSTDTC"
  )

  raw_source <- list(EX = EX)

  observed_min <- oak_cal_ref_dates(dm,
    der_var = "RFSTDTC",
    min_max = "min",
    ref_date_config_df = ref_date_conf_df,
    raw_source = raw_source
  )

  expected_min <- tibble::tribble(
    ~patient_number, ~RFSTDTC,
    "001",           "2022-04-25T10:15:00",
    "002",           "2022-05-26T04:59:00"
  )

  expect_identical(observed_min, expected_min)

  ref_date_conf_df_max <- tibble::tribble(
    ~raw_dataset_name, ~date_var, ~time_var, ~dformat, ~tformat, ~sdtm_var_name,
    "EX", "EX_ST_DT", "EX_ST_TM", "y-m-d", "H:M:S", "RFENDTC"
  )

  observed_max <- oak_cal_ref_dates(dm,
    der_var = "RFENDTC",
    min_max = "max",
    ref_date_config_df = ref_date_conf_df_max,
    raw_source = raw_source
  )

  expected_max <- tibble::tribble(
    ~patient_number, ~RFENDTC,
    "001",           "2022-04-26T10:20:00",
    "002",           "2022-05-26T06:23:00"
  )

  expect_identical(observed_max, expected_max)
})

test_that("oak_cal_ref_dates works with Date objects (no time)", {
  dm <- tibble::tribble(
    ~patient_number,
    "001",
    "002"
  )

  EX <- tibble::tribble(
    ~patient_number, ~EX_ST_DT,
    "001",           as.Date("04-25-22", "%m-%d-%y"),
    "001",           as.Date("04-25-22", "%m-%d-%y"),
    "002",           as.Date("05-26-22", "%m-%d-%y")
  )

  ref_date_conf_df <- tibble::tribble(
    ~raw_dataset_name, ~date_var, ~time_var, ~dformat, ~tformat, ~sdtm_var_name,
    "EX", "EX_ST_DT", NA_character_, "y-m-d", NA_character_, "RFSTDTC"
  )

  raw_source <- list(EX = EX)

  observed_min <- oak_cal_ref_dates(dm,
    der_var = "RFSTDTC",
    min_max = "min",
    ref_date_config_df = ref_date_conf_df,
    raw_source = raw_source
  )

  expected_min <- tibble::tribble(
    ~patient_number,    ~RFSTDTC,
    "001",           create_iso8601("04-25-22", .format = "m-d-y"),
    "002",           create_iso8601("05-26-22", .format = "m-d-y")
  )

  expect_identical(observed_min, expected_min)
})
