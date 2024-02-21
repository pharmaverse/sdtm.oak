ae <- data.frame(
  USUBJID = c("study123-123", "study123-124", "study123-125"),
  AESTDTC = c("2012-01-01", "2012-04-14", "2012-04-14"),
  stringsAsFactors = FALSE
)
dm <- data.frame(
  USUBJID = c("study123-123", "study123-124", "study123-125"),
  RFSTDTC = c("2012-02-01", "2012-04-14", NA_character_),
  stringsAsFactors = FALSE
)

test_that("`calculate_study_day()` works as expected for invalid input", {
  expect_error(
    calculate_study_day("a", "b", "C", "D", "E"),
    "sdtm_in is not a data frame"
  )
  expect_error(
    calculate_study_day(iris, "b", "C", "D", "E"),
    "dm_domain is not a data frame"
  )
  expect_error(
    calculate_study_day(iris, iris, "d", "e", "b"),
    "dm_domain needs to have the variable of refdt"
  )
  expect_error(
    calculate_study_day(iris, iris, "d", "Species"),
    "sdtm_in needs to have the variable of tgdt"
  )
  expect_error(
    calculate_study_day(iris, iris, "Petal.Length", "Species", "e"),
    "needs to have the variable of merge_key"
  )
  expect_error(
    calculate_study_day(iris, iris, "Petal.Length", "Species", 123L, "Species"),
    "study_day_var is not a character vector"
  )
  expect_warning(
    calculate_study_day(ae, dm, "AESTDTC", "RFSTDTC", "AENDY"),
    "Target date and the returned study day doesn't match."
  )

  dm1 <- data.frame(
    USUBJID = c("study123-123", "study123-123", "study123-125"),
    RFSTDTC = c("2012-02-01", "2012-04-14", "2012-04-14"),
    stringsAsFactors = FALSE
  )
  expect_warning(
    calculate_study_day(ae, dm1, "AESTDTC", "RFSTDTC", "AESTDY"),
    "Reference date is not unique for each patient!"
  )

  dm2 <- data.frame(
    USUBJID = c("study123-123", "study123-124", "study123-125"),
    RFSTDTC = c(123L, 456L, 789L),
    stringsAsFactors = FALSE
  )
  expect_warning(
    calculate_study_day(ae, dm2, "AESTDTC", "RFSTDTC", "AESTDY"),
    "Encountered errors when converting refdt to dates."
  )
})

test_that("`calculate_study_day()` works as expected for valid input", {
  res <- calculate_study_day(ae, dm, "AESTDTC", "RFSTDTC", "AESTDY")
  expected <- c(-31L, 1L, NA)
  expect_equal(res$AESTDY, expected, tolerance = "1.5e-08")
})
