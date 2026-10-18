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

test_that("`derive_study_day()` works as expected for invalid input", {
  expect_error(
    derive_study_day("a", "b", "C", "D", "E"),
    "sdtm_in is not a data frame"
  )
  expect_error(
    derive_study_day(iris, "b", "C", "D", "E"),
    "dm_domain is not a data frame"
  )
  expect_error(
    derive_study_day(iris, iris, "d", "e", "b"),
    "dm_domain needs to have the variable of refdt"
  )
  expect_error(
    derive_study_day(iris, iris, "d", "Species"),
    "sdtm_in needs to have the variable of tgdt"
  )
  expect_error(
    derive_study_day(iris, iris, "Petal.Length", "Species", "e"),
    "needs to have the variable of merge_key"
  )
  expect_error(
    derive_study_day(iris, iris, "Petal.Length", "Species", 123L, "Species"),
    "study_day_var is not a character vector"
  )
  expect_warning(
    derive_study_day(ae, dm, "AESTDTC", "RFSTDTC", "AENDY"),
    "Target date and the returned study day doesn't match."
  )

  dm1 <- data.frame(
    USUBJID = c("study123-123", "study123-123", "study123-125"),
    RFSTDTC = c("2012-02-01", "2012-04-14", "2012-04-14"),
    stringsAsFactors = FALSE
  )
  expect_warning(
    derive_study_day(ae, dm1, "AESTDTC", "RFSTDTC", "AESTDY"),
    "Reference date is not unique for each patient!"
  )

  dm2 <- data.frame(
    USUBJID = c("study123-123", "study123-124", "study123-125"),
    RFSTDTC = c(123L, 456L, 789L),
    stringsAsFactors = FALSE
  )
})

test_that("`calculate_study_day()` works as expected for valid input", {
  res <- derive_study_day(ae, dm, "AESTDTC", "RFSTDTC", "AESTDY")
  expected <- c(-31L, 1L, NA)
  expect_identical(res$AESTDY, expected)

  df <- data.frame(
    USUBJID = c("study123-123", "study123-124", "study123-125"),
    RFSTDTC = c("2012-02-01", "2012-04-14", NA_character_),
    AESTDTC = c("2012-01-01", "2012-04-14", "2012-04-14"),
    stringsAsFactors = FALSE
  )
  res1 <- derive_study_day(df, df, "AESTDTC", "RFSTDTC", "AESTDY")
  expect_identical(res1$AESTDY, expected)
})

test_that("`derive_study_day()` preserves partial dates and column attributes", {
  input <- data.frame(
    USUBJID = rep("S1", 8L),
    AESTDTC = c(
      "2024", "2024-06", "2024-06-UN", "2024-06-11",
      "2024-06-12T12:19:30", "2024-06-13", NA_character_, ""
    ),
    stringsAsFactors = FALSE
  )
  attr(input$AESTDTC, "label") <- "Collected start date/time"
  dm <- data.frame(USUBJID = "S1", RFSTDTC = "2024-06-12", stringsAsFactors = FALSE)

  actual <- derive_study_day(input, dm, "AESTDTC", "RFSTDTC", "AESTDY")

  expect_identical(actual[names(input)], input)
  expect_identical(actual$AESTDY, c(NA_integer_, NA_integer_, NA_integer_, -1L, 1L, 2L, NA_integer_, NA_integer_))
})

test_that("`derive_study_day()` preserves reference dates in the input", {
  input <- data.frame(
    USUBJID = c("S1", "S2", "S3", "S4", "S5"),
    AESTDTC = rep("2024-06-12T12:00", 5L),
    RFSTDTC = c("2024-06", "2024", "2024-06-12T08:00", NA_character_, "2024-06-13"),
    stringsAsFactors = FALSE
  )
  attr(input$RFSTDTC, "label") <- "Reference start date/time"

  actual <- derive_study_day(input, input, "AESTDTC", "RFSTDTC", "AESTDY")

  expect_identical(actual[names(input)], input)
  expect_identical(actual$AESTDY, c(NA_integer_, NA_integer_, 1L, NA_integer_, -1L))
})

test_that("`derive_study_day()` preserves Date columns and a non-default merge key", {
  input <- data.frame(
    SUBJID = c("S2", "S1", "S2"),
    AESTDTC = as.Date(c("2024-06-11", "2024-06-12", "2024-06-13")),
    stringsAsFactors = FALSE
  )
  attr(input$AESTDTC, "label") <- "Collected start date"
  dm <- data.frame(
    SUBJID = c("S1", "S2"),
    RFSTDTC = as.Date(c("2024-06-12", "2024-06-12")),
    stringsAsFactors = FALSE
  )

  actual <- derive_study_day(input, dm, "AESTDTC", "RFSTDTC", "AESTDY", merge_key = "SUBJID")

  expect_identical(actual[names(input)], input)
  expect_identical(actual$AESTDY, c(-1L, 1L, 2L))
})

test_that("`derive_study_day()` preserves rows with non-unique or missing reference dates", {
  input <- data.frame(
    USUBJID = c("S2", "S1", "S2", "S3"),
    AESTDTC = c("2024-06-13", "2024-06", "2024-06-12T12:00", NA_character_),
    stringsAsFactors = FALSE
  )
  dm <- data.frame(
    USUBJID = c("S1", "S1", "S2"),
    RFSTDTC = c("2024-06-11", "2024-06-12", "2024-06-12"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    actual <- derive_study_day(input, dm, "AESTDTC", "RFSTDTC", "AESTDY"),
    "Reference date is not unique for each patient!",
    fixed = TRUE
  )

  expect_identical(actual[names(input)], input)
  expect_identical(actual$AESTDY, c(2L, NA_integer_, 1L, NA_integer_))
})

test_that("`derive_study_day()` preserves an empty input's columns", {
  input <- data.frame(USUBJID = character(), AESTDTC = character(), stringsAsFactors = FALSE)
  dm <- data.frame(USUBJID = "S1", RFSTDTC = "2024-06-12", stringsAsFactors = FALSE)
  expected <- input
  expected$AESTDY <- integer()

  actual <- derive_study_day(input, dm, "AESTDTC", "RFSTDTC", "AESTDY")

  expect_identical(actual, expected)
})

test_that("`derive_study_day()` preserves the output of assign_datetime", {
  raw <- data.frame(
    oak_id = 1L:2L,
    raw_source = "AE",
    patient_number = c("S1", "S2"),
    START_DATE = c("2024-06-11", "2024-06-12"),
    START_TIME = c("23:59:59", "12:19:30"),
    stringsAsFactors = FALSE
  )
  ae <- assign_datetime(
    raw_dat = raw,
    raw_var = c("START_DATE", "START_TIME"),
    tgt_var = "AESTDTC",
    raw_fmt = c("y-m-d", "H:M:S")
  )
  dm <- data.frame(patient_number = c("S1", "S2"), RFSTDTC = c("2024-06-12", "2024-06-12"), stringsAsFactors = FALSE)

  actual <- derive_study_day(ae, dm, "AESTDTC", "RFSTDTC", "AESTDY", merge_key = "patient_number")

  expect_identical(as.character(ae$AESTDTC), c("2024-06-11T23:59:59", "2024-06-12T12:19:30"))
  expect_identical(actual[names(ae)], ae)
  expect_identical(actual$AESTDY, c(-1L, 1L))
})
