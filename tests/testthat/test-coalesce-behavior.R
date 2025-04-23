#
# Unit test for testing coalescing behavior in:
#
#  - `harcode_no_ct()`
#  - `harcode_ct()`
#  - `assign_no_ct()`
#  - `assign_ct()`
#
# Issue 110: https://github.com/pharmaverse/sdtm.oak/issues/110
#

cm_raw <- tibble::tibble(
  PATNUM = c(rep(375L, 2), 376L, rep(377L, 4), rep(378L, 4), rep(379L, 3)),
  `IT.CMTRT` = c(
    "BABY ASPIRIN", "CORTISPORIN", "ASPIRIN", "DIPHENHYDRAMINE HCL",
    "PARCETEMOL", "VOMIKIND", NA, "AMITRYPTYLINE", "BENADRYL",
    "DIPHENHYDRAMINE HYDROCHLORIDE", "TETRACYCLINE", "BENADRYL", "SOMINEX",
    "ZQUILL"
  ),
  `IT.CMTRTOTH` = c("Other Treatment - ", rep(NA, 5), "Other Treatment - Baby Aspirin", rep(NA, 7)),
  `IT.CMINDC` = c(
    NA, "NAUSEA", "ANEMIA", "NAUSEA", "PYREXIA", "VOMITINGS", NA,
    "COLD", "FEVER", NA, "FEVER", "COLD", "COLD", "PAIN"
  ),
  `IT.CMINDCOTH` = c("Other Indication - Vomitting", "Other Indication Fever", rep(NA, 7),
                     "Other Indication - Diarrhoea", rep(NA, 4)),
  `IT.CMDSTXT` = c("10", "50", NA, "50", NA, "One", NA, "12", "100", "Two", "10", "12", "3", "5"),
  `IT.CMDSTXTO` = c("Other Dose - 100", NA, "Other Dose - 300", NA, "Other Dose - 500", "Other Dose - 600", rep(NA, 8)),
  `IT.CMDOSU` = c("mg", "Gram", NA, "mg", "mg", "Tablet", NA, "g", "mg", NA, "mg", "IU", "mL", "%"),
  `IT.DOSUO` = c(rep(NA, 8), "Other Dose Unit", "cap", rep(NA, 4))
)

cm_raw <- sdtm.oak:::generate_oak_id_vars(cm_raw, pat_var = "PATNUM", raw_src = "cm_raw")

study_ct <- tibble::tribble(
  ~codelist_code, ~term_code, ~term_value, ~collected_value, ~term_preferred_term,     ~term_synonyms,
  "C71620",       "C25613",   "%",         "%",              "Percentage",             "Percentage",
  "C71620",       "C28253",   "mg",        "mg",             "Milligram",              "Milligram",
  "C71620",       "C28254",   "mL",        "mL",             "Milliliter",             "cm3; Milliliter",
  "C71620",       "C48155",   "g",         "g",              "Gram",                   "Gram",
  "C71620",       "C48480",   "CAPSULE",   "Capsule",        "Capsule Dosing Unit",    "cap; Capsule Dosing Unit",
  "C71620",       "C48542",   "TABLET",    "Tablet",         "Tablet Dosing Unit",     "tab; Tablet Dosing Unit",
  "C71620",       "C48579",   "IU",        "IU",             "International Unit",     "IE; International Unit"
)

cm_actual <-
  # Derive topic variable
  # Map CMTRT using assign_no_ct, raw_var=IT.CMTRT,tgt_var=CMTRT
  assign_no_ct(
    raw_dat = cm_raw,
    raw_var = "IT.CMTRT",
    tgt_var = "CMTRT"
  ) |>
  #example for assign_no_ct
  # Map CMTRT using assign_no_ct, raw_var=IT.CMTRTOTH,tgt_var=CMTRT
  assign_no_ct(
    raw_dat = cm_raw,
    raw_var = "IT.CMTRTOTH",
    tgt_var = "CMTRT"
  ) |>
  # Map CMCAT = "General Concomitant Medications" using hardcode_no_ct
  hardcode_no_ct(
    tgt_val = "General Concomitant Medications",
    raw_dat = cm_raw,
    raw_var = "IT.CMTRT",
    tgt_var = "CMCAT"
  ) |>
  # Map CMCAT = "Other General Concomitant Medications" using hardcode_no_ct
  # example for hardcode_no_ct
  hardcode_no_ct(
    tgt_val = "Other General Concomitant Medications",
    raw_dat = cm_raw,
    raw_var = "IT.CMTRTOTH",
    tgt_var = "CMCAT"
  ) |>
  # Map CMOCCUR = "Y" using hardcode_ct when raw_var = IT.CMTRT
  hardcode_ct(
    tgt_var = "CMOCCUR",
    raw_dat = cm_raw,
    raw_var = "IT.CMTRT",
    tgt_val = "Y",
    ct_spec = study_ct,
    ct_clst = "C71620"
  ) |>
  # Map CMPRESP = "Y" using hardcode_ct when raw_var = IT.CMTRTOTH
  # example for hardcode_ct
  hardcode_ct(
    tgt_var = "CMPRESP",
    raw_dat = cm_raw,
    raw_var = "IT.CMTRTOTH",
    tgt_val = "Y",
    ct_spec = study_ct,
    ct_clst = "C71620"
  ) |>
  # Map CMINDC using assign_no_ct, raw_var=IT.CMINDC,tgt_var=CMINDC
  assign_no_ct(
    raw_dat = cm_raw,
    raw_var = "IT.CMINDC",
    tgt_var = "CMINDC",
    id_vars = oak_id_vars()
  ) |>
  #example for assign_no_ct
  # Map CMINDC using assign_no_ct, raw_var=IT.CMINDCOTH,tgt_var=CMINDC
  assign_no_ct(
    raw_dat = cm_raw,
    raw_var = "IT.CMINDCOTH",
    tgt_var = "CMINDC",
    id_vars = oak_id_vars()
  ) |>
  # Map CMDOSTXT using condition_add and assign_no_ct, raw_var=IT.CMDSTXT,tgt_var=CMDOS
  # If IT.CMDSTXT is numeric, map it to CMDOS
  assign_no_ct(
    raw_dat = condition_add(cm_raw, grepl("^-?\\d*(\\.\\d+)?(e[+-]?\\d+)?$", IT.CMDSTXT)),
    raw_var = "IT.CMDSTXT",
    tgt_var = "CMDOS",
    id_vars = oak_id_vars()
  ) |>
  # Map qualifier CMDOSTXT using condition_add & assign_no_ct, raw_var=IT.CMDSTXT,tgt_var=CMDOSTXT
  # If IT.CMDSTXT is character, map it to CMDOSTXT
  assign_no_ct(
    raw_dat = condition_add(cm_raw, grepl("[^0-9eE.-]", IT.CMDSTXT)),
    raw_var = "IT.CMDSTXT",
    tgt_var = "CMDOSTXT",
    id_vars = oak_id_vars()
  ) |>
  # Map qualifier CMDOSTXT using condition_add & assign_no_ct, raw_var=IT.CMDSTXT,tgt_var=CMDOSTXT
  # If IT.CMDSTXT is character, map it to CMDOSTXT
  assign_no_ct(
    raw_dat = condition_add(cm_raw, !is.na(IT.CMDSTXTO)),
    raw_var = "IT.CMDSTXTO",
    tgt_var = "CMDOSTXT",
    id_vars = oak_id_vars()
  ) |>
  assign_ct(
    raw_dat = cm_raw,
    raw_var = "IT.DOSUO",
    tgt_var = "CMDOSU",
    ct_spec = study_ct,
    ct_clst = "C71620",
    id_vars = oak_id_vars()
  ) |>
  # Map CMDOSU and apply CT using assign_ct, raw_var=IT.CMDOSU,tgt_var=CMDOSU
  assign_ct(
    raw_dat = cm_raw,
    raw_var = "IT.CMDOSU",
    tgt_var = "CMDOSU",
    ct_spec = study_ct,
    ct_clst = "C71620",
    id_vars = oak_id_vars()
  )

cm_expected <-
  tibble::tribble(
    ~oak_id, ~raw_source, ~patient_number, ~CMTRT,                         ~CMCAT,                                    ~CMOCCUR, ~CMPRESP, ~CMINDC,                        ~CMDOS, ~CMDOSTXT,          ~CMDOSU,
    1L,      "cm_raw",    375L,            "BABY ASPIRIN",                   "General Concomitant Medications",       "Y",      "Y",      "Other Indication - Vomitting", "10",   "Other Dose - 100", "mg",
    2L,      "cm_raw",    375L,            "CORTISPORIN",                    "General Concomitant Medications",       "Y",       NA,      "NAUSEA",                       "50",   NA,                 "g",
    3L,      "cm_raw",    376L,            "ASPIRIN",                        "General Concomitant Medications",       "Y",       NA,      "ANEMIA",                       NA,     "Other Dose - 300", NA,
    4L,      "cm_raw",    377L,            "DIPHENHYDRAMINE HCL",            "General Concomitant Medications",       "Y",       NA,      "NAUSEA",                       "50",   NA,                 "mg",
    5L,      "cm_raw",    377L,            "PARCETEMOL",                     "General Concomitant Medications",       "Y",       NA,      "PYREXIA",                      NA,     "Other Dose - 500", "mg",
    6L,      "cm_raw",    377L,            "VOMIKIND",                       "General Concomitant Medications",       "Y",       NA,      "VOMITINGS",                    NA,     "One",              "TABLET",
    7L,      "cm_raw",    377L,            "Other Treatment - Baby Aspirin", "Other General Concomitant Medications", NA,       "Y",      NA,                             NA,      NA,                NA,
    8L,      "cm_raw",    378L,            "AMITRYPTYLINE",                  "General Concomitant Medications",       "Y",       NA,      "COLD",                         "12",    NA,                "g",
    9L,      "cm_raw",    378L,            "BENADRYL",                       "General Concomitant Medications",       "Y",       NA,      "FEVER",                        "100",   NA,                "OTHER DOSE UNIT",
    10L,     "cm_raw",    378L,            "DIPHENHYDRAMINE HYDROCHLORIDE",  "General Concomitant Medications",       "Y",       NA,      "Other Indication - Diarrhoea", NA,      NA,                "CAPSULE",
    11L,     "cm_raw",    378L,            "TETRACYCLINE",                   "General Concomitant Medications",       "Y",       NA,      "FEVER",                        "10",    NA,                "mg",
    12L,     "cm_raw",    379L,            "BENADRYL",                       "General Concomitant Medications",       "Y",       NA,      "COLD",                         "12",    NA,                "IU",
    13L,     "cm_raw",    379L,            "SOMINEX",                        "General Concomitant Medications",       "Y",       NA,      "COLD",                         "3",     NA,                "mL",
    14L,     "cm_raw",    379L,            "ZQUILL",                         "General Concomitant Medications",       "Y",       NA,      "PAIN",                         "5",     NA,                "%"
  )

test_that("coalesce behavior works", {
  expect_equal(cm_actual, cm_expected)
})
