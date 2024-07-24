dm <- tibble::tribble(
  ~USUBJID, ~RFSTDTC, ~RFXSTDTC,
  "test_study-375", "2020-09-28T10:10", "2020-09-28T10:10",
  "test_study-376", "2020-09-21T11:00", "2020-09-21T11:00",
  "test_study-377", NA, NA,
  "test_study-378", "2020-01-20T10:00", "2020-01-20T10:00",
  "test_study-379", NA, NA
)

sdtm_in <-
  tibble::tribble(
    ~DOMAIN, ~oak_id, ~raw_source, ~patient_number, ~USUBJID, ~VSDTC, ~VSTESTCD, ~VSORRES, ~VSSTAT, ~VISIT,
    "VS", 1L, "VTLS1", 375L, "test_study-375", "2020-09-01T13:31", "DIABP", "90", NA, "SCREENING",
    "VS", 2L, "VTLS1", 375L, "test_study-375", "2020-10-01T11:20", "DIABP", "90", NA, "SCREENING",
    "VS", 1L, "VTLS1", 375L, "test_study-375", "2020-09-28T10:10", "PULSE", "ND", NA, "SCREENING",
    "VS", 2L, "VTLS1", 375L, "test_study-375", "2020-10-01T13:31", "PULSE", "85", NA, "SCREENING",
    "VS", 1L, "VTLS2", 375L, "test_study-375", "2020-09-28T10:10", "SYSBP", "120", NA, "SCREENING",
    "VS", 2L, "VTLS2", 375L, "test_study-375", "2020-09-28T10:05", "SYSBP", "120", NA, "SCREENING",
    "VS", 1L, "VTLS1", 376L, "test_study-376", "2020-09-20", "DIABP", "75", NA, "SCREENING",
    "VS", 1L, "VTLS1", 376L, "test_study-376", "2020-09-20", "PULSE", NA, "NOT DONE", "SCREENING", # nolint
    "VS", 2L, "VTLS1", 376L, "test_study-376", "2020-09-20", "PULSE", "110", NA, "SCREENING",
    "VS", 2L, "VTLS1", 378L, "test_study-378", "2020-01-20T10:00", "PULSE", "110", NA, "SCREENING",
    "VS", 3L, "VTLS1", 378L, "test_study-378", "2020-01-21T11:00", "PULSE", "105", NA, "SCREENING"
  )

d <- list(sdtm_in = sdtm_in, dm = dm)


test_that("derive_blfl example works", {
  observed_output <- derive_blfl(
    sdtm_in = d$sdtm_in,
    dm_domain = d$dm,
    tgt_var = "VSLOBXFL",
    ref_var = "RFXSTDTC",
    baseline_visits = c("SCREENING")
  )
  observed_output

  expect_snapshot_value(observed_output, style = "json2")
})

test_that("derive_blfl sdmt_in validations work", {
  sdmt_in_no_domain <-
    d$sdtm_in |>
    dplyr::select(-DOMAIN)

  expect_snapshot_error(derive_blfl(
    sdtm_in = sdmt_in_no_domain,
    dm_domain = d$dm,
    tgt_var = "VSLOBXFL",
    ref_var = "RFXSTDTC"
  ))

  sdmt_in_no_id_vars <-
    d$sdtm_in |>
    dplyr::select(-sdtm.oak::oak_id_vars())

  expect_snapshot_error(derive_blfl(
    sdtm_in = sdmt_in_no_id_vars,
    dm_domain = d$dm,
    tgt_var = "VSLOBXFL",
    ref_var = "RFXSTDTC"
  ))

  sdmt_in_no_vs_vars <-
    d$sdtm_in |>
    dplyr::select(-c(
      "VSORRES",
      "VSSTAT",
      "VSTESTCD",
      "VSDTC"
    ))

  expect_snapshot_error(derive_blfl(
    sdtm_in = sdmt_in_no_vs_vars,
    dm_domain = d$dm,
    tgt_var = "VSLOBXFL",
    ref_var = "RFXSTDTC"
  ))
})

test_that("derive_blfl dm_domain validations work", {
  dm_no_vars <-
    d$dm |>
    dplyr::select(-c(RFXSTDTC, USUBJID))

  expect_snapshot_error(derive_blfl(
    sdtm_in = d$sdtm_in,
    dm_domain = dm_no_vars,
    tgt_var = "VSLOBXFL",
    ref_var = "RFXSTDTC"
  ))
})

test_that("derive_blfl tgt_var and ref_var validations work", {
  expect_snapshot_error(derive_blfl(
    sdtm_in = d$sdtm_in,
    dm_domain = d$dm,
    tgt_var = list("bad"),
    ref_var = "RFXSTDTC"
  ))

  expect_snapshot_error(derive_blfl(
    sdtm_in = d$sdtm_in,
    dm_domain = d$dm,
    tgt_var = "VSLOBXFL",
    ref_var = d$dm
  ))

  expect_snapshot_error(derive_blfl(
    sdtm_in = d$sdtm_in,
    dm_domain = d$dm,
    tgt_var = "DMLOBXFL",
    ref_var = "RFXSTDTC"
  ))
})

test_that("derive_blfl DOMAIN validation works", {
  sdtm_in_bad_domain <-
    d$sdtm_in |>
    dplyr::mutate(DOMAIN = 4L)

  expect_snapshot_error(derive_blfl(
    sdtm_in = sdtm_in_bad_domain,
    dm_domain = d$dm,
    tgt_var = "VSLOBXFL",
    ref_var = "RFXSTDTC"
  ))
})

test_that("`dtc_datepart`: basic usage", {
  expect_identical(
    dtc_datepart(
      c(NA, "", "2021", "2021-12", "2021-12-25", "2021-12-25T12:00:00")
    ),
    c(NA, NA, NA, NA, "2021-12-25", "2021-12-25")
  )

  ## Prevent partial or missing dates from being set to NA
  expect_identical(
    dtc_datepart(
      c(NA, "", "2021", "2021-12", "2021-12-25", "2021-12-25T12:00:00"),
      partial_as_na = FALSE
    ),
    c(NA, "", "2021", "2021-12", "2021-12-25", "2021-12-25")
  )
})

test_that("`dtc_timepart`: basic usage", {
  # Partial or missing times set to NA and seconds ignored by default
  expect_identical(dtc_timepart(
    c(
      NA,
      "",
      "2021-12-25",
      "2021-12-25T12",
      "2021-12-25T12:30",
      "2021-12-25T12:30:59"
    )
  ), c(NA, NA, NA, NA, "12:30", "12:30"))

  # Prevent partial or missing times from being set to NA
  expect_identical(
    dtc_timepart(
      c(
        NA,
        "",
        "2021-12-25",
        "2021-12-25T12",
        "2021-12-25T12:30",
        "2021-12-25T12:30:59"
      ),
      partial_as_na = FALSE
    ),
    c(NA, "", "", "12", "12:30", "12:30")
  )

  # Do not ignore seconds, partial or missing times set to NA
  expect_identical(
    dtc_timepart(
      c(
        NA,
        "",
        "2021-12-25",
        "2021-12-25T12",
        "2021-12-25T12:30",
        "2021-12-25T12:30:59"
      ),
      ignore_seconds = FALSE
    ),
    c(NA, NA, NA, NA, NA, "12:30:59")
  )

  # Do not ignore seconds and prevent partial or missing times from being set to NA
  expect_identical(
    dtc_timepart(
      c(
        NA,
        "",
        "2021-12-25",
        "2021-12-25T12",
        "2021-12-25T12:30",
        "2021-12-25T12:30:59"
      ),
      partial_as_na = FALSE,
      ignore_seconds = FALSE
    ),
    c(NA, "", "", "12", "12:30", "12:30:59")
  )
})
