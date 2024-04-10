test_that("ct_spec_vars() works as expected", {
  expect_identical(
    ct_spec_vars(),
    c(
      "codelist_code",
      "collected_value",
      "term_synonyms",
      "term_value"
    )
  )

  expect_identical(
    ct_spec_vars(set = "all"),
    c(
      "codelist_code",
      "collected_value",
      "term_synonyms",
      "term_value"
    )
  )

  expect_identical(
    ct_spec_vars(set = "ct_clst"),
    "codelist_code"
  )

  expect_identical(
    ct_spec_vars(set = "from"),
    c(
      "collected_value",
      "term_synonyms"
    )
  )

  expect_identical(ct_spec_vars(set = "to"), "term_value")
})

test_that("ct_spec_vars() fails with invalid input choice", {
  expect_error(ct_spec_vars("foo"))
  expect_error(ct_spec_vars(1L))
  expect_error(ct_spec_vars(FALSE))
  expect_error(ct_spec_vars(NULL))
})

test_that("assert_ct_spec() works as expected", {
  # Load an example controlled terminology spec.
  ct_spec <- read_ct_spec_example("ct-01-cm")
  cols <- colnames(ct_spec)
  ct_clst_col <- ct_spec_vars("ct_clst")
  to_col <- ct_spec_vars("to")

  expect_no_error(assert_ct_spec(ct_spec, optional = FALSE))
  expect_no_error(assert_ct_spec(ct_spec, optional = TRUE))
  expect_identical(assert_ct_spec(ct_spec, optional = FALSE), ct_spec)
  expect_identical(assert_ct_spec(ct_spec, optional = TRUE), ct_spec)
  expect_null(assert_ct_spec(NULL, optional = TRUE))

  # Codelist code column is one of the key variables that must be present
  # in `ct_spec`, so being missing should trigger an error.
  expect_error(assert_ct_spec(ct_spec[setdiff(cols, ct_clst_col)], optional = FALSE))
  expect_error(assert_ct_spec(ct_spec[setdiff(cols, ct_clst_col)], optional = TRUE))

  # The codelist code and the "to" columns of a controlled terminology should
  # not contain NAs, as otherwise the mapping is undefined. If that happens
  # an error is triggered.
  ct_spec01 <- ct_spec
  ct_spec01[[ct_clst_col]] <- NA_character_
  expect_error(assert_ct_spec(ct_spec01, optional = FALSE))
  expect_error(assert_ct_spec(ct_spec01, optional = TRUE))

  ct_spec02 <- ct_spec
  ct_spec02[[to_col]] <- NA_character_
  expect_error(assert_ct_spec(ct_spec01, optional = FALSE))
  expect_error(assert_ct_spec(ct_spec01, optional = TRUE))

  ct_spec_empty <-
    data.frame(
      codelist_code = character(),
      collected_value = character(),
      term_synonyms = character(),
      term_value = character(),
      stringsAsFactors = FALSE
    )

  # `ct_spec` cannot be empty as that means that there are no mappings.
  expect_error(assert_ct_spec(ct_spec_empty, optional = TRUE))
  expect_error(assert_ct_spec(ct_spec_empty, optional = FALSE))
})

test_that("assert_ct_clst() works as expected", {
  # Read in a controlled terminology example.
  ct_spec <- read_ct_spec_example("ct-01-cm")

  # If `ct_clst` is not supplied and is not optional, then it should err.
  expect_error(assert_ct_clst(
    ct_spec = NULL,
    ct_clst = NULL,
    optional = FALSE
  ))

  # If `ct_clst` is not supplied but it is optional, then all fine.
  expect_no_error(assert_ct_clst(
    ct_spec = NULL,
    ct_clst = NULL,
    optional = TRUE
  ))
  # Moreover, in case of no error, `ct_clst` should be returned invisibly, in this
  # case `NULL`.
  expect_null(assert_ct_clst(
    ct_spec = NULL,
    ct_clst = NULL,
    optional = TRUE
  ))

  # If `ct_clst` is supplied but `ct_spec` is not, then err.
  expect_error(assert_ct_clst(
    ct_spec = NULL,
    ct_clst = "C71113",
    optional = FALSE
  ))
  expect_error(assert_ct_clst(
    ct_spec = NULL,
    ct_clst = "C71113",
    optional = TRUE
  ))

  # If `ct_spec` is supplied but `ct_clst` is NULL, then err if `ct_clst` is not optional, or
  # return `ct_clst` invisibly.
  expect_error(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = NULL,
    optional = FALSE
  ))
  expect_no_error(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = NULL,
    optional = TRUE
  ))
  expect_null(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = NULL,
    optional = TRUE
  ))

  # If both `ct_spec` and `ct_clst` are supplied, then `ct_spec` must be a valid controlled
  # terminology data set and `ct_clst` must contain a codelist code available among
  # the possibilities in column `codelist_code` (as returned by `ct_spec_vars("ct_clst")`).
  expect_error(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = "foo",
    optional = FALSE
  ))
  expect_error(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = "",
    optional = FALSE
  ))

  expect_error(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = NA_character_,
    optional = FALSE
  ))
  expect_error(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = NA_character_,
    optional = TRUE
  ))

  expect_identical(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = "C71113",
    optional = FALSE
  ), "C71113")
  expect_identical(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = "C66726",
    optional = FALSE
  ), "C66726")
  expect_identical(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = "C71113",
    optional = TRUE
  ), "C71113")
  expect_identical(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = "C66726",
    optional = TRUE
  ), "C66726")
})

test_that("assert_ct_clst(): when ct_spec is empty", {
  ct_spec <-
    data.frame(
      codelist_code = character(),
      collected_value = character(),
      term_synonyms = character(),
      term_value = character(),
      stringsAsFactors = FALSE
    )

  # If `ct_spec` is supplied but `ct_clst` is NULL, then err if `ct_clst` is not optional, or
  # return `ct_clst` invisibly.
  expect_error(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = NULL,
    optional = FALSE
  ))
  expect_no_error(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = NULL,
    optional = TRUE
  ))
  expect_null(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = NULL,
    optional = TRUE
  ))

  # If both `ct_spec` and `ct_clst` are supplied, then `ct_spec` must be a valid controlled
  # terminology data set and `ct_clst` must contain a codelist code available among
  # the possibilities in column `codelist_code` (as returned by `ct_spec_vars("ct_clst")`).
  expect_error(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = "foo",
    optional = FALSE
  ))
  expect_error(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = "",
    optional = FALSE
  ))

  expect_error(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = NA_character_,
    optional = FALSE
  ))
  expect_error(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = NA_character_,
    optional = TRUE
  ))

  expect_error(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = "C71113",
    optional = FALSE
  ))
  expect_error(assert_ct_clst(
    ct_spec = ct_spec,
    ct_clst = "C71113",
    optional = TRUE
  ))
})

test_that("ct_mappings(): works as expected", {
  ct_spec <- read_ct_spec_example("ct-01-cm")
  ct_spec_qd <- dplyr::filter(ct_spec, term_code == "C25473")

  expect_identical(
    ct_mappings(ct_spec = ct_spec_qd),
    tibble::tibble(
      from = c("QD", "QD (Every Day)", "/day", "Daily", "Per Day"),
      to = rep("QD", 5L)
    )
  )
})
