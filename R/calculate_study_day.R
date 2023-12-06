#' `calculate_study_day` Performs the Study Day Calculation
#'
calculate_study_day <- function(ds_in,
                                ds_ref,
                                refdt,
                                tgdt,
                                merge_key = "USUBJID") {

  assertthat::assert_that(is.data.frame(ds_in))
  assertthat::assert_that(is.data.frame(ds_ref))
  assertthat::assert_that(hasName(ds_ref, refdt))
  assertthat::assert_that(hasName(ds_in, tgdt))
  assertthat::assert_that(hasName(ds_ref, merge_key))
  assertthat::assert_that(hasName(ds_in, merge_key))

  if (!identical(ds_in, ds_ref)) {
    ds_ref <- unique(ds_ref[c(merge_key, refdt)])

    check_refdt_uniqueness <- ds_ref %>%
      dplyr::group_by(dplyr::pick({{merge_key}})) %>%
      dplyr::filter(n() > 1)
    if (nrow(check_refdt_uniqueness) > 0) {
      stop("Reference date is not unique for each patient!")
    }

    ds_in <- ds_in %>%
      dplyr::left_join(ds_ref, by = structure(names = merge_key, .Data = merge_key))
  }

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

