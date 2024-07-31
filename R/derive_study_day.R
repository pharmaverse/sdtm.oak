#' `derive_study_day` performs study day calculation
#' @description
#' This function takes the an input data frame and a reference data frame (which
#' is DM domain in most cases), and calculate the study day from reference date
#' and target date. In case of unexpected conditions like reference date is not
#' unique for each patient, or reference and input dates are not actual dates,
#' NA will be returned for those records.
#'
#' @param sdtm_in Input data frame that contains the target date.
#' @param dm_domain Reference date frame that contains the reference date.
#' @param tgdt Target date from `sdtm_in` that will be used to calculate the study
#' day.
#' @param refdt Reference date from `dm_domain` that will be used as reference to
#' calculate the study day.
#' @param study_day_var New study day variable name in the output. For
#' example, AESTDY for AE domain and CMSTDY for CM domain.
#' @param merge_key Character to represent the merging key between `sdtm_in` and
#' `dm_domain`.
#'
#' @return Data frame that takes all columns from `sdtm_in` and a new variable
#' to represent the calculated study day.
#'
#' @export
#'
#' @examples
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
#' derive_study_day(ae, dm, "AESTDTC", "RFSTDTC", "AESTDY")
#'
derive_study_day <- function(sdtm_in,
                             dm_domain,
                             tgdt,
                             refdt,
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
  if (gsub("DTC", "", tgdt, fixed = TRUE) != gsub("DY", "", study_day_var, fixed = TRUE)) {
    cli::cli_warn(
      paste(
        "Target date and the returned study day doesn't match.",
        "Expecting matching date and study day, for example, CMENDTC and CMENDY"
      )
    )
  }

  original_variables <- names(sdtm_in)

  if (!identical(sdtm_in, dm_domain)) {
    dm_domain <- unique(dm_domain[c(merge_key, refdt)])

    check_refdt_uniqueness <- dm_domain |>
      dplyr::group_by(dplyr::pick({{ merge_key }})) |>
      dplyr::filter(dplyr::n() > 1L)
    if (nrow(check_refdt_uniqueness) > 0L) {
      cli::cli_warn(
        paste(
          "Reference date is not unique for each patient!",
          "Patient without unique reference date will be ingored.",
          "NA will be returned for such records."
        )
      )
      dm_domain <- dm_domain[
        !dm_domain[[merge_key]] %in% check_refdt_uniqueness[[merge_key]],
      ]
    }

    sdtm_in <- sdtm_in |>
      dplyr::left_join(
        dm_domain,
        by = merge_key
      )
  }

  # convert to character to verify the iso format
  sdtm_in[[refdt]] <- as.character(sdtm_in[[refdt]])
  sdtm_in[[tgdt]] <- as.character(sdtm_in[[tgdt]])

  # refdt/tgdt should be in ISO format, otherwise throw warning
  sdtm_in[[refdt]] <- tryCatch(
    as.Date(sdtm_in[[refdt]], "%Y-%m-%d"),
    error = function(e) {
      cli::cli_warn(
        paste(
          "Encountered errors when converting refdt to dates.",
          "The warning message is",
          e$message
        ),
        call = NULL
      )
      sdtm_in[[refdt]]
    }
  )
  sdtm_in[[tgdt]] <- tryCatch(
    as.Date(sdtm_in[[tgdt]], "%Y-%m-%d"),
    error = function(e) {
      cli::cli_warn(
        paste(
          "Encountered errors when converting tgdt to dates.",
          "The warning message is",
          e$message
        ),
        call = NULL
      )
      sdtm_in[[tgdt]]
    }
  )

  ref <- as.Date(sdtm_in[[refdt]])
  tgt <- as.Date(sdtm_in[[tgdt]])

  # SDTMIG 4.4.4 Use of the Study Day Variables
  res <- ifelse(tgt >= ref, tgt - ref + 1L, tgt - ref)

  sdtm_in <- sdtm_in[original_variables]
  sdtm_in[study_day_var] <- as.integer(res)
  return(sdtm_in)
}
