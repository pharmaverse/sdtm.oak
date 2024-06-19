raw_dataset <-
  tibble::tribble(
    ~patnum, ~MDRAW,
    101L, "BABY ASPIRIN",
    102L, "CORTISPORIN",
    103L, NA_character_,
    104L, "DIPHENHYDRAMINE HCL"
  )

# generate a test case using testthat for generate_oak_id_vars function using raw_dataset

testthat::test_that("check if generate_oak_id_vars function works", {
  observed_output <- generate_oak_id_vars(
    raw_dat = raw_dataset,
    pat_var = "patnum",
    raw_src = "Concomitant Medication"
  )

  expected_output <- tibble::tribble(
    ~oak_id, ~raw_source, ~patient_number, ~patnum, ~MDRAW,
    1L, "Concomitant Medication", 101L, 101L, "BABY ASPIRIN",
    2L, "Concomitant Medication", 102L, 102L, "CORTISPORIN",
    3L, "Concomitant Medication", 103L, 103L, NA,
    4L, "Concomitant Medication", 104L, 104L, "DIPHENHYDRAMINE HCL"
  )

  expect_identical(observed_output, expected_output)
})

test_that("check inputs to the function", {
  expect_error(generate_oak_id_vars(
    raw_dat = raw_dataset,
    pat_var = c("patnum", "patient_number"),
    raw_src = "Concomitant Medication"
  ))

  expect_error(generate_oak_id_vars(
    raw_dat = raw_dataset,
    pat_var = "patnum",
    raw_src = c("Concomitant Medication", "cm")
  ))
})
