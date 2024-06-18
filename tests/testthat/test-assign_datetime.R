test_that("assign_datetime: date and time conversion", {
  md1 <-
    tibble::tribble(
      ~oak_id, ~raw_source, ~patient_number, ~MDBDR,        ~MDEDR,        ~MDETM,
      1L,      "MD1",       375L,            NA,            NA,            NA,
      2L,      "MD1",       375L,            "15-Sep-20",   NA,            NA,
      3L,      "MD1",       376L,            "17-Feb-21",   "17-Feb-21",   NA,
      4L,      "MD1",       377L,            "4-Oct-20",    NA,            NA,
      5L,      "MD1",       377L,            "20-Jan-20",   "20-Jan-20",   "10:00:00",
      6L,      "MD1",       377L,            "UN-UNK-2019", "UN-UNK-2019", NA,
      7L,      "MD1",       377L,            "20-UNK-2019", "20-UNK-2019", NA,
      8L,      "MD1",       378L,            "UN-UNK-2020", "UN-UNK-2020", NA,
      9L,      "MD1",       378L,            "26-Jan-20",   "26-Jan-20",   "07:00:00",
      10L,     "MD1",       378L,            "28-Jan-20",   "1-Feb-20",    NA,
      11L,     "MD1",       378L,            "12-Feb-20",   "18-Feb-20",   NA,
      12L,     "MD1",       379L,            "10-UNK-2020", "20-UNK-2020", NA,
      13L,     "MD1",       379L,            NA,            NA,            NA,
      14L,     "MD1",       379L,            NA,            "17-Feb-20",   NA
    )

  warning_msg <-
    r"{There were 12 parsing problems\. Run `problems\(\)` on parsed results for details\.}"
  expect_warning(rlang::with_interactive(
    assign_datetime(
      tgt_var = "CMSTDTC",
      raw_dat = md1,
      raw_var = c("MDEDR", "MDETM"),
      raw_fmt = c("d-m-y", "H:M:S"),
      raw_unk = c("UN", "UNK")
    )
  ), regexp = warning_msg)

  # If not run interactively then warnings should not be raised.
  expect_silent(
    cm1 <- assign_datetime(
      tgt_var = "CMSTDTC",
      raw_dat = md1,
      raw_var = c("MDEDR", "MDETM"),
      raw_fmt = c("d-m-y", "H:M:S"),
      raw_unk = c("UN", "UNK")
    )
  )

  problems_index <- seq(1L, 14L)[-c(5L, 9L)]
  problems <- tibble::tibble(
    ..i = problems_index,
    MDEDR = md1$MDEDR[problems_index],
    MDETM = md1$MDETM[problems_index]
  )

  cmstdtc <-
    structure(
      c(
        NA,
        NA,
        "2021-02-17",
        NA,
        "2020-01-20T10:00:00",
        "2019",
        "2019---20",
        "2020",
        "2020-01-26T07:00:00",
        "2020-02-01",
        "2020-02-18",
        "2020---20",
        NA,
        "2020-02-17"
      ),
      class = "iso8601",
      problems = problems
    )

  expected <-
    cm1 |>
    dplyr::select(dplyr::all_of(c("oak_id", "raw_source", "patient_number"))) |>
    dplyr::bind_cols(tibble::tibble(CMSTDTC = cmstdtc))

  expect_identical(object = cm1, expected = expected)
})
