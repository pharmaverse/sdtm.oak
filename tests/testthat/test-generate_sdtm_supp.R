dm <- read_domain_example("dm")[1L:5L, ]
supp_qual_info <- read.csv(system.file("spec/suppqual_spec.csv", package = "sdtm.oak"))

test_that("`generate_sdtm_supp` works as expected", {
  dm_suppdm <-
    generate_sdtm_supp(
      dm,
      idvar = NULL,
      supp_qual_info = supp_qual_info,
      qnam = "Variable",
      label_var = "Label",
      orig_var = "Origin"
    )

  expected_dm_output <- dm |> dplyr::select(-dplyr::all_of((supp_qual_info$Variable)))

  expect_identical(dm_suppdm$DM, expected_dm_output, ignore_attr = TRUE)

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
  ) |>
    dplyr::mutate(dplyr::across(c("IDVAR", "QEVAL"), as.character),
      IDVARVAL = as.integer(IDVARVAL)
    )
  # nolint end

  expect_identical(dm_suppdm$SUPPDM, expected_suppdm_output, ignore_attr = TRUE)
})

test_that("`generate_sdtm_supp` input validation works", {
  expect_error(
    generate_sdtm_supp(
      dm,
      idvar = 123L,
      supp_qual_info = supp_qual_info,
      qnam = "Variable",
      label_var = "Label",
      orig_var = "Origin"
    ),
    "Argument `idvar` must be a scalar"
  )

  expect_error(
    generate_sdtm_supp(
      dm,
      idvar = NULL,
      supp_qual_info = "abc",
      qnam = "Variable",
      label_var = "Label",
      orig_var = "Origin"
    ),
    "Argument `supp_qual_info` must be class"
  )

  expect_error(
    generate_sdtm_supp(
      dm,
      idvar = NULL,
      supp_qual_info = supp_qual_info,
      qnam = "Var",
      label_var = "Label",
      orig_var = "Origin"
    ),
    "Required variable `Var`"
  )

  expect_error(
    generate_sdtm_supp(
      dm,
      idvar = NULL,
      supp_qual_info = supp_qual_info,
      qnam = "Variable",
      label_var = c("label", "qlabel"),
      orig_var = "Origin"
    ),
    "Argument `label_var` must be a scalar"
  )

  expect_error(
    generate_sdtm_supp(
      dm |> dplyr::select(-COMPLT16),
      idvar = NULL,
      supp_qual_info = supp_qual_info,
      qnam = "Variable",
      label_var = "Label",
      orig_var = "Origin"
    ),
    "Required variable `COMPLT16` is missing"
  )
})


test_that("`generate_sdtm_supp` honours a supplied `idvar` and custom `label_var`/`orig_var` (#148)", {
  vs <- data.frame(
    STUDYID = "S1",
    DOMAIN = "VS",
    USUBJID = c("S1-1", "S1-1", "S1-2"),
    VSSEQ = c(1L, 2L, 1L),
    VSPOS = c("SITTING", NA, "STANDING"),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    Variable = "VSPOS",
    L = "Vital Signs Position of Subject",
    O = "CRF",
    stringsAsFactors = FALSE
  )
  expect_message(
    vs_suppvs <-
      generate_sdtm_supp(
        vs,
        idvar = "VSSEQ",
        supp_qual_info = spec,
        qnam_var = "Variable",
        label_var = "L",
        orig_var = "O"
      ),
    "QEVAL should be manually programmed"
  )
  expect_named(vs_suppvs, c("VS", "SUPPVS"))
  expect_identical(vs_suppvs$VS, vs[, c("STUDYID", "DOMAIN", "USUBJID", "VSSEQ")], ignore_attr = TRUE)
  expected_suppvs <- tibble::tibble(
    STUDYID = "S1",
    RDOMAIN = "VS",
    USUBJID = c("S1-1", "S1-2"),
    IDVAR = "VSSEQ",
    IDVARVAL = c(1L, 1L),
    QNAM = "VSPOS",
    QLABEL = "Vital Signs Position of Subject",
    QVAL = c("SITTING", "STANDING"),
    QORIG = "CRF",
    QEVAL = NA_character_
  )
  expect_identical(vs_suppvs$SUPPVS, expected_suppvs, ignore_attr = TRUE)
})
