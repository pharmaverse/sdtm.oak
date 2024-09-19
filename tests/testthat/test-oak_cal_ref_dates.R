dm <- tibble::tribble(
  ~patient_number,   ~USUBJID, ~SUBJID, ~SEX,
            "001", "XXXX-001",   "001",  "F",
            "002", "XXXX-002",   "002",  "M",
            "003", "XXXX-003",   "003",  "M"
  )

expected <- tibble::tribble(
  ~patient_number,   ~USUBJID, ~SUBJID, ~SEX,           ~RFSTDTC,            ~RFENDTC,
            "001", "XXXX-001",   "001",  "F", "2023-05-15T08:19",        "2023-06-11",
            "002", "XXXX-002",   "002",  "M",       "2023-07-25",  "2023-11-03T20:30",
            "003", "XXXX-003",   "003",  "M",      NA_character_,      NA_character_
  )

ref_date_conf_df <- tibble::tribble(
  ~dataset_name,   ~date_var,     ~time_var,         ~dformat,      ~tformat, ~sdtm_var_name,
          "EX1", "EX_ST_DT1",   "EX_ST_TM1",     "dd-mm-yyyy",         "H:M",      "RFSTDTC",
          "EX2", "EX_ST_DT2", NA_character_,    "dd-mmm-yyyy", NA_character_,      "RFSTDTC",
          "EX1", "EX_EN_DT1",   "EX_EN_TM1",     "dd-mm-yyyy",         "H:M",      "RFENDTC",
          "EX2", "EX_ST_DT2", NA_character_,    "dd-mmm-yyyy", NA_character_,      "RFENDTC"
  )

EX1 <- tibble::tribble(
  ~patient_number,   ~EX_ST_DT1,     ~EX_EN_DT1, ~EX_ST_TM1,    ~EX_EN_TM1,
            "001", "15-05-2023",   "15-05-2023",    "10:20",       "11:00",
            "001", "15-05-2023",   "15-05-2023",     "9:15",       "10:00",
            "001", "15-05-2023",   "15-05-2023",     "8:19",       "09:00",
            "002", "02-10-2023",   "02-10-2023",  "UNK:UNK", NA_character_,
            "002", "0l-11-2023",   "03-11-2023",    "11:19",       "20:30"
  )

EX2 <-  tibble::tribble(
  ~patient_number,     ~EX_ST_DT2,
            "001",  "11-JUN-2023",
            "002",  "24-OCT-2023",
            "002",  "25-JUL-2023",
            "002",  "30-OCT-2023",
            "002", "UNK-OCT-2023"
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

