test_that("`oak_id_vars`: basic usage", {
  expect_identical(
    oak_id_vars(),
    c("oak_id", "raw_source", "patient_number")
  )

  expect_identical(
    oak_id_vars(extra_vars = "sample_id"),
    c("oak_id", "raw_source", "patient_number", "sample_id")
  )
})

test_that("`contains_oak_id_vars()`: basic usage", {
  expect_true(contains_oak_id_vars(oak_id_vars()))

  expect_false(contains_oak_id_vars(character()))

  expect_false(contains_oak_id_vars(c("oak_id", "raw_source")))
})
