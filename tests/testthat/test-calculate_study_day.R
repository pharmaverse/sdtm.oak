AE <- data.frame(
  USUBJID = c("study123-123", "study123-124", "study123-125"),
  AESTDTC = c("2012-01-01", "2012-04-14", "2012-04-14")
)
DM <- data.frame(
  USUBJID = c("study123-123", "study123-124", "study123-125"),
  RFSTDTC = c("2012-02-01", "2012-04-14", NA_character_)
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
    calculate_study_day(iris, iris, "c", "d", "e"),
    "dm_domain needs to have the variable of refdt"
  )
  expect_error(
    calculate_study_day(iris, iris, "Species", "d", "e"),
    "sdtm_in needs to have the variable of tgdt"
  )
  expect_error(
    calculate_study_day(iris, iris, "Species", "Petal.Length", "e"),
    "needs to have the variable of merge_key"
  )
  expect_error(
    calculate_study_day(iris, iris, "Species", "Petal.Length", 123, "Species"),
    "study_day_var is not a character vector"
  )
  expect_warning(
    calculate_study_day(AE, DM, "RFSTDTC", "AESTDTC", "AENDY"),
    "Target date and the returned study day doesn't match."
  )

  DM1 <- data.frame(
    USUBJID = c("study123-123", "study123-123", "study123-125"),
    RFSTDTC = c("2012-02-01", "2012-04-14", "2012-04-14")
  )
  expect_warning(
    calculate_study_day(AE, DM1, "RFSTDTC", "AESTDTC", "AESTDY"),
    "Reference date is not unique for each patient!"
  )

  DM2 <- data.frame(
    USUBJID = c("study123-123", "study123-124", "study123-125"),
    RFSTDTC = c(123, 456, 789)
  )
  expect_warning(
    calculate_study_day(AE, DM2, "RFSTDTC", "AESTDTC", "AESTDY"),
    "Encountered errors when converting refdt to dates."
  )
})

test_that("`calculate_study_day()` works as expected for valid input", {
  res <- calculate_study_day(AE, DM, "RFSTDTC", "AESTDTC", "AESTDY")
  expected <- c(-31, 1, NA)
  expect_equal(res$AESTDY, expected)
})
