dm <- read_domain_example("dm")[1L:5L, ]
spec <- read.csv(system.file("spec/suppqual_spec.csv", package = "sdtm.oak"))

test_that("`gen_sdtm_supp` works as expected", {
  final <-
    gen_sdtm_supp(
      dm,
      idvar = NULL,
      spec = spec,
      qnam = "Variable",
      label_var = "Label",
      orig_var = "Origin"
    )

  expected_dm_output <- dm |> dplyr::select(-dplyr::all_of((spec$Variable)))

  expect_equal(final$DM, expected_dm_output)

  # nolint start
  expected_suppdm_output <- tibble::tribble(
    ~STUDYID,       ~RDOMAIN,      ~USUBJID, ~IDVAR, ~IDVARVAL,      ~QNAM,                                 ~QLABEL, ~QVAL,    ~QORIG, ~QEVAL,
    "CDISCPILOT01",     "DM", "01-701-1015",     NA,        NA, "COMPLT16", "Completers of Week 16 Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1015",     NA,        NA, "COMPLT24", "Completers of Week 16 Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1015",     NA,        NA,  "COMPLT8", "Completers of Week 16 Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1015",     NA,        NA, "EFFICACY",              "Efficacy Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1015",     NA,        NA,      "ITT",       "Intent to Treat Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1015",     NA,        NA,   "SAFETY",                "Safety Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1023",     NA,        NA, "EFFICACY",              "Efficacy Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1023",     NA,        NA,      "ITT",       "Intent to Treat Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1023",     NA,        NA,   "SAFETY",                "Safety Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1028",     NA,        NA, "COMPLT16", "Completers of Week 16 Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1028",     NA,        NA, "COMPLT24", "Completers of Week 16 Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1028",     NA,        NA,  "COMPLT8", "Completers of Week 16 Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1028",     NA,        NA, "EFFICACY",              "Efficacy Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1028",     NA,        NA,      "ITT",       "Intent to Treat Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1028",     NA,        NA,   "SAFETY",                "Safety Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1033",     NA,        NA, "EFFICACY",              "Efficacy Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1033",     NA,        NA,      "ITT",       "Intent to Treat Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1033",     NA,        NA,   "SAFETY",                "Safety Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1034",     NA,        NA, "COMPLT16", "Completers of Week 16 Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1034",     NA,        NA, "COMPLT24", "Completers of Week 16 Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1034",     NA,        NA,  "COMPLT8", "Completers of Week 16 Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1034",     NA,        NA, "EFFICACY",              "Efficacy Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1034",     NA,        NA,      "ITT",       "Intent to Treat Population Flag",   "Y", "DERIVED",     NA,
    "CDISCPILOT01",     "DM", "01-701-1034",     NA,        NA,   "SAFETY",                "Safety Population Flag",   "Y", "DERIVED",     NA
  )
  # nolint end

  expect_equal(final$SUPPDM, expected_suppdm_output)

})

test_that("`gen_sdtm_supp` input validation works", {
  expect_error(
    gen_sdtm_supp(
      dm,
      idvar = 123L,
      spec = spec,
      qnam = "Variable",
      label_var = "Label",
      orig_var = "Origin"
    ),
    "Argument `idvar` must be a scalar"
  )

  expect_error(
    gen_sdtm_supp(
      dm,
      idvar = NULL,
      spec = "abc",
      qnam = "Variable",
      label_var = "Label",
      orig_var = "Origin"
    ),
    "Argument `spec` must be class"
  )

  expect_error(
    gen_sdtm_supp(
      dm,
      idvar = NULL,
      spec = spec,
      qnam = "Var",
      label_var = "Label",
      orig_var = "Origin"
    ),
    "Required variable `Var`"
  )

  expect_error(
    gen_sdtm_supp(
      dm,
      idvar = NULL,
      spec = spec,
      qnam = "Variable",
      label_var = c("label", "qlabel"),
      orig_var = "Origin"
    ),
    "Argument `label_var` must be a scalar"
  )

  expect_error(
    gen_sdtm_supp(
      dm |> dplyr::select(-COMPLT16),
      idvar = NULL,
      spec = spec,
      qnam = "Variable",
      label_var = "Label",
      orig_var = "Origin"
    ),
    "Required variable `COMPLT16` is missing"
  )
})
