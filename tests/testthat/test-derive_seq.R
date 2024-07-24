test_that("`derive_seq()` works as expected", {
  # Test for VSSEQ derivation
  vs <- read_domain_example("vs")

  rec_vars <- c("STUDYID", "USUBJID", "VSTESTCD", "VSDTC", "VSTPTNUM")
  observed_vsseq <- derive_seq(tgt_dat = vs, tgt_var = "VSSEQ", rec_vars = rec_vars)

  # nolint start
  expected_vsseq <- tibble::tribble(
    ~STUDYID, ~DOMAIN,     ~USUBJID,                  ~VSSPID, ~VSTESTCD,             ~VSDTC, ~VSTPTNUM, ~VSSEQ,
    "ABC123",    "VS", "ABC123-375", "/F:VTLS1-D:9795532-R:2",   "DIABP", "2020-09-01T13:31",        NA,     1L,
    "ABC123",    "VS", "ABC123-375", "/F:VTLS2-D:9795533-R:2",   "DIABP", "2020-09-28T11:00",         2,     2L,
    "ABC123",    "VS", "ABC123-375", "/F:VTLS1-D:9795532-R:2",    "TEMP", "2020-09-01T13:31",        NA,     3L,
    "ABC123",    "VS", "ABC123-375", "/F:VTLS2-D:9795533-R:2",    "TEMP", "2020-09-28T11:00",         2,     4L,
    "ABC123",    "VS", "ABC123-376", "/F:VTLS1-D:9795591-R:1",   "DIABP",       "2020-09-20",        NA,     1L,
    "ABC123",    "VS", "ABC123-376", "/F:VTLS1-D:9795591-R:1",    "TEMP",       "2020-09-20",        NA,     2L
  )
  # nolint end

  expect_identical(observed_vsseq, expected_vsseq)

  # Test for APSEQ derivation
  apsc <- read_domain_example("apsc")

  observed_apseq <- derive_seq(
    tgt_dat = apsc,
    tgt_var = "APSEQ",
    rec_vars = c("STUDYID", "RSUBJID", "SCTESTCD"),
    sbj_vars = c("STUDYID", "RSUBJID")
  )

  expected_apseq <- tibble::tribble(
    ~STUDYID,     ~RSUBJID,  ~SCTESTCD, ~DOMAIN,     ~SREL,           ~SCCAT, ~APSEQ,
    "ABC123", "ABC123-210", "EDULEVEL",  "APSC",  "FRIEND", "CAREGIVERSTUDY",     1L,
    "ABC123", "ABC123-210", "LVSBJIND",  "APSC",  "FRIEND", "CAREGIVERSTUDY",     2L,
    "ABC123", "ABC123-210",   "TMSPPT",  "APSC",  "FRIEND", "CAREGIVERSTUDY",     3L,
    "ABC123", "ABC123-211",  "CAREDUR",  "APSC", "SIBLING", "CAREGIVERSTUDY",     1L,
    "ABC123", "ABC123-211", "LVSBJIND",  "APSC", "SIBLING", "CAREGIVERSTUDY",     2L,
    "ABC123", "ABC123-212",  "JOBCLAS",  "APSC",  "SPOUSE", "CAREGIVERSTUDY",     1L
  )

  expect_identical(observed_apseq, expected_apseq)
})

test_that("`is_seq_name()`: basic usage", {
  expect_true(is_seq_name("AESEQ"))

  expect_false(is_seq_name("AEseq"))

  expect_false(is_seq_name("AESEQUENCE"))
})
