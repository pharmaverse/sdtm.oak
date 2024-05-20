dta <- function(env = parent.frame()) {
  dm <- tibble::tribble(
    ~USUBJID,           ~RFSTDTC,          ~RFXSTDTC,
    "test_study-375", "2020-09-28T10:10", "2020-09-28T10:10",
    "test_study-376", "2020-09-21T11:00", "2020-09-21T11:00",
    "test_study-377",                 NA,                 NA,
    "test_study-378", "2020-01-20T10:00", "2020-01-20T10:00",
    "test_study-379",                 NA,                 NA,
  )

  sdtm_in <-
    tibble::tribble(
      ~DOMAIN, ~oak_id, ~raw_source, ~patient_number,      ~USUBJID,             ~VSDTC, ~VSTESTCD, ~VSORRES, ~VSSTAT,
      "VS",      1L,     "VTLS1",            375L, "test_study-375", "2020-09-01T13:31",   "DIABP",     "90",      NA,
      "VS",      2L,     "VTLS1",            375L, "test_study-375", "2020-10-01T11:20",   "DIABP",     "90",      NA,
      "VS",      1L,     "VTLS1",            375L, "test_study-375", "2020-09-28T10:10",   "PULSE",     "ND",      NA,
      "VS",      2L,     "VTLS1",            375L, "test_study-375", "2020-10-01T13:31",   "PULSE",     "85",      NA,
      "VS",      1L,     "VTLS2",            375L, "test_study-375", "2020-09-28T10:10",   "SYSBP",     "120",      NA,
      "VS",      2L,     "VTLS2",            375L, "test_study-375", "2020-09-28T10:05",   "SYSBP",     "120",      NA,
      "VS",      1L,     "VTLS1",            376L, "test_study-376",       "2020-09-20",   "DIABP",     "75",      NA,
      "VS",      1L,     "VTLS1",            376L, "test_study-376",       "2020-09-20",   "PULSE",      NA,      "NOT DONE", #nolint
      "VS",      2L,     "VTLS1",            376L, "test_study-376",       "2020-09-20",   "PULSE",     "110",      NA
    )

  withr::defer({
    rm(d, envir = env)
  }, envir = env)

  list(sdtm_in = sdtm_in, dm = dm)
}

test_that("derive_blfl example works", {
 d <- dta()

 observed_output <- derive_blfl(sdtm_in = d$sdtm_in,
                                dm_domain = d$dm,
                                tgt_var = "VSLOBXFL",
                                ref_var = "RFXSTDTC")
 observed_output

 expect_snapshot_value(observed_output, style = "json2")
})

test_that("derive_blfl sdmt_in validations work", {
  d <- dta()
  sdmt_in_noDOMAIN <-
    d$sdtm_in |>
    dplyr::select(-DOMAIN)

  expect_snapshot_error(derive_blfl(sdtm_in = sdmt_in_noDOMAIN,
                                    dm_domain = d$dm,
                                    tgt_var = "VSLOBXFL",
                                    ref_var = "RFXSTDTC"))

  sdmt_in_noIDvars <-
    d$sdtm_in |>
    dplyr::select(-sdtm.oak:::oak_id_vars())

  expect_snapshot_error(derive_blfl(sdtm_in = sdmt_in_noIDvars,
                                    dm_domain = d$dm,
                                    tgt_var = "VSLOBXFL",
                                    ref_var = "RFXSTDTC"))

  sdmt_in_noVSvars <-
    d$sdtm_in |>
    dplyr::select(-c("VSORRES",
                     "VSSTAT",
                     "VSTESTCD",
                     "VSDTC"))

  expect_snapshot_error(derive_blfl(sdtm_in = sdmt_in_noVSvars,
                                    dm_domain = d$dm,
                                    tgt_var = "VSLOBXFL",
                                    ref_var = "RFXSTDTC"))
})

test_that("derive_blfl dm_domain validations work", {
  d <- dta()

  dm_noVars <-
    d$dm |>
    dplyr::select(-c(RFXSTDTC, USUBJID))

  expect_snapshot_error(derive_blfl(sdtm_in = d$sdtm_in,
                                    dm_domain = dm_noVars,
                                    tgt_var = "VSLOBXFL",
                                    ref_var = "RFXSTDTC"))
})

test_that("derive_blfl tgt_var and ref_var validations work", {
  d <- dta()

  expect_snapshot_error(derive_blfl(sdtm_in = d$sdtm_in,
                                    dm_domain = d$dm,
                                    tgt_var = list("bad"),
                                    ref_var = "RFXSTDTC"))

  expect_snapshot_error(derive_blfl(sdtm_in = d$sdtm_in,
                                    dm_domain = d$dm,
                                    tgt_var = "VSLOBXFL",
                                    ref_var = d$dm))

  expect_snapshot_error(derive_blfl(sdtm_in = d$sdtm_in,
                                    dm_domain = d$dm,
                                    tgt_var = "DMLOBXFL",
                                    ref_var = "RFXSTDTC"))
})

test_that("derive_blfl DOMAIN validation works", {
  d <- dta()

  sdtm_in_badDOMAIN <-
    d$sdtm_in |>
    dplyr::mutate(DOMAIN = 4)

  expect_snapshot_error(derive_blfl(sdtm_in = sdtm_in_badDOMAIN,
                                    dm_domain = d$dm,
                                    tgt_var = "VSLOBXFL",
                                    ref_var = "RFXSTDTC"))
})
