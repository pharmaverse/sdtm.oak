# `aesos`: example raw data set.
aesos <- tibble::tribble(
  ~oak_id, ~raw_source, ~patient_number, ~AESO, ~AESOSP, ~AESEV, ~AESER, ~AETERM,
  1L, "RS1", 101L, 0L, "Pain", "Mild", "No", "Headache",
  2L, "RS1", 102L, 0L, NA, "Severe", "Yes", "Dizziness",
  3L, "RS2", 103L, 1L, NA, "Moderate", "No", NA,
  4L, "RS2", 104L, 1L, NA, "Mild", "No", "Eye issues",
  5L, "RS3", 105L, 1L, "Nausea", "Severe", "Yes", "Food Poisoning"
)

# `oe_inter`: example target data set.
oe_inter <- tibble::tribble(
  ~oak_id, ~raw_source, ~patient_number,
  1L,      "RS1",       101L,
  3L,      "RS2",       103L,
  4L,      "RS2",       104L,
  5L,      "RS3",       105L
)

test_that("hardcode_no_ct works as expected", {
  aesos_cnd <- condition_add(aesos, AESO == 1L & !is.na(AESOSP))

  result <- hardcode_no_ct(
    raw_dat = aesos_cnd,
    raw_var = "AESO",
    tgt_var = "OEORRES",
    tgt_val = "Y",
    tgt_dat = oe_inter
  )

  expected_result <- tibble::tribble(
    ~oak_id, ~raw_source, ~patient_number, ~OEORRES,
    # NA because `aesos_cnd` is conditioned to be FALSE on this record.
    1L, "RS1", 101L, NA_character_,
    # NA because `aesos_cnd` is conditioned to be FALSE on this record.
    3L, "RS2", 103L, NA_character_,
    # NA because `aesos_cnd` is conditioned to be FALSE on this record.
    4L, "RS2", 104L, NA_character_,
    # Successful derivation
    5L, "RS3", 105L, "Y"
  )

  expect_identical(result, expected_result)
})

test_that("hardcode_ct works as expected", {
  aesos_cnd <- condition_add(aesos, AESO == 1L & is.na(AESOSP))
  ct_spec <- tibble::tibble(
    codelist_code = "C117743",
    term_code = "C178048",
    CodedData = "HYPERMIA",
    term_value = "HYPERMIA",
    collected_value = "IOISYMPO",
    term_synonyms = "IOISYMPO"
  )

  result <-
    hardcode_ct(
      raw_dat = aesos_cnd,
      raw_var = "AETERM",
      tgt_var = "OETESTCD",
      tgt_val = "IOISYMPO",
      ct_spec = ct_spec,
      ct_clst = "C117743",
      tgt_dat = oe_inter
    )

  expected_result <- tibble::tribble(
    ~oak_id, ~raw_source, ~patient_number, ~OETESTCD,
    # `NA` because `aesos_cnd` is conditioned to be FALSE for this record.
    1L, "RS1", 101L, NA_character_,
    # `NA` because AETERM == NA for this record in `aesos_cnd`.
    3L, "RS2", 103L, NA_character_,
    # Successful derivation: IOISYMPO -> HYPERMIA.
    4L, "RS2", 104L, "HYPERMIA",
    # `NA` because `aesos_cnd` is conditioned to be FALSE for this record.
    5L, "RS3", 105L, NA_character_
  )

  expect_identical(result, expected_result)
})
