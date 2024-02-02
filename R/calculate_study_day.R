#' `calculate_study_day` performs study day calculation
#' @description
#' This function takes the an input data frame and a reference data frame (which
#' is DM domain in most cases), and calculate the study day from reference date
#' and target date. In case of unexpected conditions like reference date is not
#' unique for each patient, or reference and input dates are not actual dates,
#' NA will be returned for those records.
#'
#' @md
#' @param sdtm_in Input data frame that contains the target date.
#' @param dm_domain reference date.frame that contains the reference date.
#' @param refdt reference date from `dm_domain` that will be used as reference to
#' calculate the study day.
#' @param tgdt target date from `sdtm_in` that will be used to calcualte the study
#' day.
#' @param study_day_var the new study day variable name in the output. For
#' example, AESTDY for AE domain for CMSTDY for CM domain.
#' @param merge_key character to represents the merging key between `sdtm_in` and
#' `dm_domain`.
#'
#' @return a data.frame that takes all columns from `sdtm_in` and a new variable
#' to represent the calculated study day.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ae <- data.frame(
#'   USUBJID = c("study123-123", "study123-124", "study123-125"),
#'   AESTDTC = c("2012-01-01", "2012-04-14", "2012-04-14")
#' )
#' dm <- data.frame(
#'   USUBJID = c("study123-123", "study123-124", "study123-125"),
#'   RFSTDTC = c("2012-02-01", "2012-04-14", NA)
#' )
#' ae$AESTDTC <- as.Date(ae$AESTDTC)
#' dm$RFSTDTC <- as.Date(dm$RFSTDTC)
#' calculate_study_day(ae, dm, "RFSTDTC", "AESTDTC", "AESTDY")
#' }
#'
calculate_study_day <- function(sdtm_in,
                                dm_domain = DM,
                                refdt = "RFSTDTC",
                                tgdt,
                                study_day_var,
                                merge_key = "USUBJID") {
  assertthat::assert_that(is.data.frame(sdtm_in))
  assertthat::assert_that(is.data.frame(dm_domain))
  assertthat::assert_that(
    utils::hasName(dm_domain, refdt),
    msg = "dm_domain needs to have the variable of refdt."
  )
  assertthat::assert_that(
    utils::hasName(sdtm_in, tgdt),
    msg = "sdtm_in needs to have the variable of tgdt."
  )
  assertthat::assert_that(
    utils::hasName(dm_domain, merge_key),
    msg = "dm_domain needs to have the variable of merge_key."
  )
  assertthat::assert_that(
    utils::hasName(sdtm_in, merge_key),
    msg = "sdtm_in needs to have the variable of merge_key."
  )
  assertthat::assert_that(is.character(study_day_var))
  # check tgdt and study_day_var matching, for example, CMSTDTC matches CMSTDY
  if (gsub("DTC", "", tgdt) != gsub("DY", "", study_day_var)) {
    warning(
      "Target date and the returned study day doesn't match. ",
      "Expecting matching date and study day, for example, CMENDTC and CMENDY"
    )
  }

  original_variables <- names(sdtm_in)

  if (!identical(sdtm_in, dm_domain)) {
    dm_domain <- unique(dm_domain[c(merge_key, refdt)])

    check_refdt_uniqueness <- dm_domain %>%
      dplyr::group_by(dplyr::pick({{ merge_key }})) %>%
      dplyr::filter(dplyr::n() > 1)
    if (nrow(check_refdt_uniqueness) > 0) {
      warning(
        "Reference date is not unique for each patient! ",
        "Patient without unique reference date will be ingored. ",
        "NA will be returned for such records."
      )
      dm_domain <- dm_domain[
        !dm_domain[[merge_key]] %in% check_refdt_uniqueness[[merge_key]],
      ]
    }

    sdtm_in <- sdtm_in %>%
      dplyr::left_join(
        dm_domain,
        by = structure(names = merge_key, .Data = merge_key)
      )
  }

  # refdt/tgdt should be in ISO format, otherwise throw warning
  sdtm_in[[refdt]] <- tryCatch(
    as.Date(sdtm_in[[refdt]], "%Y-%m-%d"),
    error = function(e) {
      warning(
        "Encountered errors when converting refdt to dates. ",
        "The warning message is ",
        e$message,
        call. = FALSE
      )
      NA
    }
  )
  sdtm_in[[tgdt]] <- tryCatch(
    as.Date(sdtm_in[[tgdt]], "%Y-%m-%d"),
    error = function(e) {
      warning(
        "Encountered errors when converting tgdt to dates. ",
        "The warning message is ",
        e$message,
        call. = FALSE
      )
      NA
    }
  )

  refdt_vector <- sdtm_in[[refdt]]
  tgdt_vector <- sdtm_in[[tgdt]]

  res <- ifelse(
    test = refdt_vector <= tgdt_vector,
    yes = refdt_vector - tgdt_vector + 1,
    no = ifelse(
      test = refdt_vector > tgdt_vector,
      yes = tgdt_vector - refdt_vector,
      no = NA
    )
  )

  sdtm_in <- sdtm_in[original_variables]
  sdtm_in[study_day_var] <- res
  return(sdtm_in)
}
