#' `calculate_study_day` Performs the Study Day Calculation
#'
calculate_study_day <- function(ds_in, refdt, tgdt) {
  assertthat::assert_that(is.data.frame(ds_in))
  assertthat::assert_that(hasName(ds_in, refdt))
  assertthat::assert_that(hasName(ds_in, tgdt))

  # question: should I assume that refdt/tgdt was converted to Date already?
  # If assume that refdt and tgdt are already dates
  if (!("Date" %in% class(ds_in[[refdt]]) && "Date" %in% class(ds_in[[tgdt]]))) {
    stop("Reference and target date has to be Date objects.")
  }
  refdt_vector <- ds_in[[refdt]]
  tgdt_vector <- ds_in[[tgdt]]

  res <- ifelse(
    test = refdt_vector <= tgdt_vector,
    yes = refdt_vector - tgdt_vector + 1,
    no = ifelse(
      test = refdt_vector > tgdt_vector,
      yes = tgdt_vector - refdt_vector,
      no = NA
    )
  )
  return(res)
}

