test_that("ct_vars() works as expected", {
  expect_identical(
    ct_vars(),
    c(
      "codelist_code",
      "collected_value",
      "term_synonyms",
      "term_value"
    )
  )

  expect_identical(
    ct_vars(set = "all"),
    c(
      "codelist_code",
      "collected_value",
      "term_synonyms",
      "term_value"
    )
  )

  expect_identical(
    ct_vars(set = "cl"),
    "codelist_code"
  )

  expect_identical(
    ct_vars(set = "from"),
    c(
      "collected_value",
      "term_synonyms"
    )
  )

  expect_identical(ct_vars(set = "to"), "term_value")
})

test_that("ct_vars() fails with invalid input choice", {
  expect_error(ct_vars("foo"))
  expect_error(ct_vars(1L))
  expect_error(ct_vars(FALSE))
  expect_error(ct_vars(NULL))
})

test_that("assert_cl() works as expected", {
  # Read in a controlled terminology example.
  ct <- read_ct_example("ct-01-cm")

  # If `cl` is not supplied and is not optional, then it should err.
  expect_error(assert_cl(
    ct = NULL,
    cl = NULL,
    optional = FALSE
  ))

  # If `cl` is not supplied but it is optional, then all fine.
  expect_no_error(assert_cl(
    ct = NULL,
    cl = NULL,
    optional = TRUE
  ))
  # Moreover, in case of no error, `cl` should be returned invisibly, in this
  # case `NULL`.
  expect_null(assert_cl(
    ct = NULL,
    cl = NULL,
    optional = TRUE
  ))

  # If `cl` is supplied but `ct` is not, then err.
  expect_error(assert_cl(
    ct = NULL,
    cl = "C71113",
    optional = FALSE
  ))
  expect_error(assert_cl(
    ct = NULL,
    cl = "C71113",
    optional = TRUE
  ))

  # If `ct` is supplied but `cl` is NULL, then err if `cl` is not optional, or
  # return `cl` invisibly.
  expect_error(assert_cl(
    ct = ct,
    cl = NULL,
    optional = FALSE
  ))
  expect_no_error(assert_cl(
    ct = ct,
    cl = NULL,
    optional = TRUE
  ))
  expect_null(assert_cl(
    ct = ct,
    cl = NULL,
    optional = TRUE
  ))

  # If both `ct` and `cl` are supplied, then `ct` must be a valid controlled
  # terminology data set and `cl` must contain a code-list code available among
  # the possibilities in column `codelist_code` (as returned by `ct_vars("cl")`).
  expect_error(assert_cl(
    ct = ct,
    cl = "foo",
    optional = FALSE
  ))
  expect_error(assert_cl(
    ct = ct,
    cl = "",
    optional = FALSE
  ))

  expect_error(assert_cl(
    ct = ct,
    cl = NA_character_,
    optional = FALSE
  ))
  expect_error(assert_cl(
    ct = ct,
    cl = NA_character_,
    optional = TRUE
  ))

  expect_identical(assert_cl(
    ct = ct,
    cl = "C71113",
    optional = FALSE
  ), "C71113")
  expect_identical(assert_cl(
    ct = ct,
    cl = "C66726",
    optional = FALSE
  ), "C66726")
  expect_identical(assert_cl(
    ct = ct,
    cl = "C71113",
    optional = TRUE
  ), "C71113")
  expect_identical(assert_cl(
    ct = ct,
    cl = "C66726",
    optional = TRUE
  ), "C66726")
})

test_that("assert_cl(): when ct is empty", {
  ct <-
    data.frame(
      codelist_code = character(),
      collected_value = character(),
      term_synonyms = character(),
      term_value = character(),
      stringsAsFactors = FALSE
    )

  # If `ct` is supplied but `cl` is NULL, then err if `cl` is not optional, or
  # return `cl` invisibly.
  expect_error(assert_cl(
    ct = ct,
    cl = NULL,
    optional = FALSE
  ))
  expect_no_error(assert_cl(
    ct = ct,
    cl = NULL,
    optional = TRUE
  ))
  expect_null(assert_cl(
    ct = ct,
    cl = NULL,
    optional = TRUE
  ))

  # If both `ct` and `cl` are supplied, then `ct` must be a valid controlled
  # terminology data set and `cl` must contain a code-list code available among
  # the possibilities in column `codelist_code` (as returned by `ct_vars("cl")`).
  expect_error(assert_cl(
    ct = ct,
    cl = "foo",
    optional = FALSE
  ))
  expect_error(assert_cl(
    ct = ct,
    cl = "",
    optional = FALSE
  ))

  expect_error(assert_cl(
    ct = ct,
    cl = NA_character_,
    optional = FALSE
  ))
  expect_error(assert_cl(
    ct = ct,
    cl = NA_character_,
    optional = TRUE
  ))

  expect_error(assert_cl(
    ct = ct,
    cl = "C71113",
    optional = FALSE
  ))
  expect_error(assert_cl(
    ct = ct,
    cl = "C71113",
    optional = TRUE
  ))
})
