test_that("ct_vars works as expected", {

  expect_equal(ct_vars(),
               c(
                 "codelist_code",
                 "collected_value",
                 "term_synonyms",
                 "term_value"
               ))

  expect_equal(
    ct_vars(set = "all"),
    c(
      "codelist_code",
      "collected_value",
      "term_synonyms",
      "term_value"
    )
  )

  expect_equal(ct_vars(set = "cl"),
               "codelist_code")

  expect_equal(ct_vars(set = "from"),
               c("collected_value",
                 "term_synonyms"))

  expect_equal(ct_vars(set = "to"), "term_value")
})

test_that("ct_vars fails with invalid input choice", {

  expect_error(ct_vars("foo"))
  expect_error(ct_vars(1L))
  expect_error(ct_vars(FALSE))
  expect_error(ct_vars(NULL))
})
