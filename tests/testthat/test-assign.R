test_that("assign_ct works as expected with a conditioned `tgt_dat`", {
  vs_raw_dat <- tibble::tibble(
    oak_id = 1L:5L,
    raw_source = c("VS1", "VS2", "VS3", "VS4", "VS5"),
    patient_number = c(101L, 102L, 103L, 104L, 105L),
    TEMPLOC = c("Oral", "Axillary", "Rectal", "Tympanic", "Temporal")
  )

  vs_tgt_dat <- tibble::tibble(
    oak_id = as.integer(rep(1L:5L, each = 4L)),
    raw_source = rep(c("VS1", "VS2", "VS3", "VS4", "VS5"), each = 4L),
    patient_number = as.integer(rep(c(101L, 102L, 103L, 104L, 105L), each = 4L)),
    VSTESTCD = c(
      "TEMP", "BPSYS", "BPDIAS", "HR",
      "TEMP", "BPSYS", "BPDIAS", "HR",
      "TEMP", "BPSYS", "BPDIAS", "HR",
      "TEMP", "BPSYS", "BPDIAS", "HR",
      "TEMP", "BPSYS", "BPDIAS", "HR"
    )
  )

  # vital signs' locations
  vs_loc_raw <- c(
    "Mouth", "Arm", "Arm", "Arm", "Armpit", "Arm", "Arm", "Arm",
    "Rectum", "Arm", "Arm", "Arm", "auris", "Arm", "Arm", "Arm", "brow", "Arm",
    "Arm", "Arm"
  )

  vs_loc_tgt <- c(
    "ORAL",
    rep(NA, 3L),
    "AXILLA",
    rep(NA, 3L),
    "ANUS",
    rep(NA, 3L),
    "EAR",
    rep(NA, 3L),
    "FOREHEAD",
    rep(NA, 3L)
  )

  ct_spec <- tibble::tibble(
    codelist_code = "C74456",
    term_code = c("C32141", "C12674", "C12394", "C89803", "C43362"),
    CodedData = c("ARM", "AXILLA", "EAR", "FOREHEAD", "ANUS"),
    term_value = c("ARM", "AXILLA", "EAR", "FOREHEAD", "ANUS"),
    collected_value = c("Arm", "Armpit", "auris", "brow", "anus"),
    term_synonyms = c("Arm", "Axillary", "Tympanic", "Temporal", "Rectal")
  )

  result <-
    assign_ct(
      tgt_dat = condition_add(vs_tgt_dat, VSTESTCD == "TEMP"),
      tgt_var = "VSLOC",
      raw_dat = vs_raw_dat,
      raw_var = "TEMPLOC",
      ct_spec = ct_spec,
      ct_clst = "C74456"
    )

  expected_result <-
    tibble::add_column(
      vs_tgt_dat,
      VSLOC = vs_loc_tgt
    )

  expect_identical(result, expected_result)
})


test_that("assign_ct works as expected with both `raw_dat` and `tgt_dat` as conditioned data frames", {
  ct_spec <- tibble::tibble(
    codelist_code = "C78734",
    term_code = c("C150895", "C12434", "C13275", "C89803", "C12801"),
    CodedData = c("SWABBED MATERIAL", "BLOOD", "SALIVA", "URINE", "TISSUE"),
    term_value = c("SWABBED MATERIAL", "BLOOD", "SALIVA", "URINE", "TISSUE"),
    collected_value = c("Nasopharyngeal Swab", "blood", "drool", "urine sample", "tissue"),
    term_synonyms = c("Swab", "Blood", "Spit", "urinary excretion", "tissue sample")
  )

  fa_raw_dat <- tibble::tibble(
    oak_id = as.integer(1L:5L),
    raw_source = c("FA1", "FA2", "FA3", "FA4", "FA5"),
    patient_number = 101L:105L,
    SPCNM = c("Nasopharyngeal Swab", "Blood", "Saliva", "Urine", "Tissue"),
    SPECTYP = c(NA, NA, "Swab", NA, NA)
  )

  fa_tgt_dat <- tibble::tibble(
    oak_id = 1L:5L,
    raw_source = c("FA1", "FA2", "FA3", "FA4", "FA5"),
    patient_number = 101L:105L,
    FATESTCD = c("STATUS", "OTHER", "STATUS", "STATUS", "OTHER"),
    FAOBJ = c(
      "Severe Acute Resp Syndrome Coronavirus 2",
      "Other Condition",
      "Severe Acute Resp Syndrome Coronavirus 2",
      "Severe Acute Resp Syndrome Coronavirus 2",
      "Other Condition"
    )
  )

  result <-
    assign_ct(
      tgt_dat = condition_add(
        fa_tgt_dat,
        FATESTCD == "STATUS" &
          FAOBJ == "Severe Acute Resp Syndrome Coronavirus 2"
      ),
      tgt_var = "FASPEC",
      raw_dat = condition_add(fa_raw_dat, is.na(SPECTYP)),
      raw_var = "SPCNM",
      ct_spec = ct_spec,
      ct_clst = "C78734"
    )

  expected_result <-
    fa_tgt_dat |>
    tibble::add_column(FASPEC = c("SWABBED MATERIAL", NA, NA, "URINE", NA))

  expect_identical(result, expected_result)
})

test_that("assign_ct works as expected with conditions across both data sets", {
  cm_raw_dat <- tibble::tibble(
    oak_id = 1L:5L,
    raw_source = paste0("MD", 1L:5L),
    patient_number = 101L:105L,
    CMMODIFY = c("ASPIRIN EC", "IBUPROFEN LYSINE", "PARACETAMOL", "DICLOFENAC", "NAPROXEN")
  )

  cm_tgt_dat <- tibble::tibble(
    oak_id = 1L:5L,
    raw_source = paste0("MD", 1L:5L),
    patient_number = 101L:105L,
    CMTRT = c("ASPIRIN", "IBUPROFEN", "PARACETAMOL", "DICLOFENAC", "NAPROXEN")
  )

  # This only works if the raw data set and the target data set have the same
  # number of records, otherwise the comparison CMMODIFY != CMTRT is not
  # meaningful.
  result1 <-
    assign_no_ct(
      tgt_dat = condition_add(cm_tgt_dat, CMMODIFY != CMTRT, .dat2 = cm_raw_dat),
      tgt_var = "CMMODIFY",
      raw_dat = cm_raw_dat,
      raw_var = "CMMODIFY"
    )

  # Because both data sets have to have the same number of records for the
  # comparison to be meaningful, then we can just as well condition the
  # raw data set itself.
  result2 <-
    assign_no_ct(
      tgt_dat = cm_tgt_dat,
      tgt_var = "CMMODIFY",
      raw_dat = condition_add(cm_raw_dat, CMMODIFY != CMTRT, .dat2 = cm_tgt_dat),
      raw_var = "CMMODIFY"
    )

  expected_result <-
    cm_tgt_dat |>
    tibble::add_column(CMMODIFY = c("ASPIRIN EC", "IBUPROFEN LYSINE", NA, NA, NA))

  expect_identical(result1, expected_result)
  expect_identical(result2, expected_result)
})
