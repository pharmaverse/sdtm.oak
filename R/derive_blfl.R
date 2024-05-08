#' Extract date part from ISO8601 date/time variable
#'
#' The date part is extracted from an ISO8601 date/time variable.
#' By default, partial or missing dates are set to NA.
#'
#' @param dtc Character vector containing ISO8601 date/times.
#' @param partial_as_na Logical `TRUE` or `FALSE` indicating whether
#'   partial dates should be set to NA (default is `TRUE`).
#'
#' @return Character vector containing ISO8601 dates.
#'
#' @examples
#' ## Partial or missing dates set to NA by default
#' dtc_datepart(
#'   c(NA, "", "2021", "2021-12", "2021-12-25", "2021-12-25T12:00:00")
#' )
#'   # |-->  c(NA, NA, NA, NA, "2021-12-25", "2021-12-25")
#'
#' ## Prevent partial or missing dates from being set to NA
#' dtc_datepart(
#'   c(NA, "", "2021", "2021-12", "2021-12-25", "2021-12-25T12:00:00"),
#'   partial_as_na = FALSE
#' )
#'   # |--> c(NA, "", "2021", "2021-12", "2021-12-25", "2021-12-25")
#'
dtc_datepart <- function(dtc, partial_as_na = TRUE) {

  # Assert that dtc is a character vector
  checkmate::assert_character(dtc)

  # Extract date part from ISO 8601 date/time variable
  dt <- sub("^([^T]+).*", "\\1", dtc)

  # Set partial or missing dates to NA, depending on partial_as_na parameter
  if (partial_as_na) {
    dt <- ifelse(nchar(dt) < 10, NA, dt)
  }

  return(dt)
}

#' Extract time part from ISO 8601 date/time variable
#'
#' The time part is extracted from an ISO 8601 date/time variable.
#' By default, partial or missing times are set to NA, and seconds are ignored
#' and not extracted.
#'
#' @param dtc Character vector containing ISO 8601 date/times.
#' @param partial_as_na Logical `TRUE` or `FALSE` indicating whether
#'   partial times should be set to NA (default is `TRUE`).
#' @param ignore_seconds Logical `TRUE` or `FALSE` indicating whether
#'   seconds should be ignored (default is `TRUE`).
#'
#' @return Character vector containing ISO 8601 times.
#'
#' @examples
#' ## Partial or missing times set to NA and seconds ignored by default
#' dtc_timepart(
#'   c(NA, "", "2021-12-25", "2021-12-25T12", "2021-12-25T12:30", "2021-12-25T12:30:59")
#' )
#'   # |--> c(NA, NA, NA, NA, "12:30", "12:30")
#'
#' ## Prevent partial or missing times from being set to NA
#' dtc_timepart(
#'   c(NA, "", "2021-12-25", "2021-12-25T12", "2021-12-25T12:30", "2021-12-25T12:30:59"),
#'   partial_as_na = FALSE
#' )
#'   # |--> c(NA, "", "", "12", "12:30", "12:30")
#'
#' ## Do not ignore seconds, partial or missing times set to NA
#' dtc_timepart(
#'   c(NA, "", "2021-12-25", "2021-12-25T12", "2021-12-25T12:30", "2021-12-25T12:30:59"),
#'   ignore_seconds = FALSE
#' )
#'   # |--> c(NA, NA, NA, NA, NA, "12:30:59")
#'
#' ## Do not ignore seconds and prevent partial or missing times from being set to NA
#' dtc_timepart(
#'   c(NA, "", "2021-12-25", "2021-12-25T12", "2021-12-25T12:30", "2021-12-25T12:30:59"),
#'   partial_as_na = FALSE,
#'   ignore_seconds = FALSE
#' )
#'   # |--> c(NA, "", "", "12", "12:30", "12:30:59")
#'
dtc_timepart <- function(dtc, partial_as_na = TRUE, ignore_seconds = TRUE) {

  # Assert that dtc is a character vector
  checkmate::assert_character(dtc)

  # Determine length of time part depending on ignore_seconds parameter
  tm_length <- ifelse(ignore_seconds, 5, 8)

  # Extract time part from ISO 8601 date/time variable
  tm <- substr(sub("^([^T]+)T?", "", dtc), 1, tm_length)

  # Set partial or missing times to NA, depending on partial_as_na parameter
  if (partial_as_na) {
    tm <- ifelse(nchar(tm) < tm_length, NA, tm)
  }

  return(tm)
}

#' Derive Baseline Flag or Last Observation Before Exposure Flag
#'
#' Derive the baseline flag variable (`--BLFL`) or the last observation before
#' exposure flag (`--LOBXFL`), from the observation date/time (`--DTC`), and a
#' DM domain reference date/time.
#'
#' The methodology and approach implemented in this function are based on
#' concepts and examples found in the Roche version of the {roak} package.
#'
#' The derivation is as follows:
#'
#'  - Remove records where the result (`--ORRES`) is missing. Also, exclude records
#'    with results labeled as "ND" (No Data) or "NOT DONE" in the `--ORRES` column,
#'    which indicate that the measurement or observation was not completed. This
#'    step is important even if a previous cleaning step (like the
#'    'oak_clean_not_done' function) might not have been applied to the data yet.
#'  - Remove records where the status (`--STAT`) indicates the observation or test
#'    was not performed, marked as "NOT DONE".
#'  - Divide the date and time column (`--DTC`) and the reference date/time
#'    variable (`reference_date_variable`) into separate date and time components. Ignore
#'    any seconds recorded in the time component, focusing only on hours and
#'    minutes for further calculations.
#'  - Set partial or missing dates to `NA`.
#'  - Set partial or missing times to `NA`.
#'  - Get a list of baseline visits from `Baseline column`
#'    (if it exists) in `oak_pkg_env$study_visit_configuration`.
#'  - Get a list of baseline timepoints from `Baseline` column
#'    (if it exists) in `oak_pkg_env$timepoint_conf`.
#'  - Filter on rows that have domain and reference dates not equal to
#'    `NA`. (Ref: **X**)
#'  - Filter **X** on rows with domain date prior to (less than)
#'    reference date. (Ref: **A**)
#'  - Filter **X** on rows with domain date equal to reference date but
#'    domain and reference times not equal to `NA` and domain time prior to (less
#'    than) reference time. (Ref: **B**)
#'  - Filter **X** on rows with domain date equal to reference date but
#'    domain and/or reference time equal to NA and:
#'      - VISIT is in baseline visits list (if it exists) and
#'      - xxTPT is in baseline timepoints list (if it exists).
#'
#' (Ref: **C**)
#'  - Combine the rows from **A**, **B**, and **C** to get a
#' data frame of pre-reference date observations. Sort the rows by `USUBJID`,
#' `--STAT`, and `--ORRES`.
#'  - Group by `USUBJID` and `--TESTCD` and filter on the rows
#' that have maximum value from `--DTC`. Keep only the oak id variables and
#' `--TESTCD` (because these are the unique values). Remove any duplicate rows.
#' Assign the baseline flag variable, `--BLFL`, the last observation before
#' exposure flag (`--LOBXFL`) variable to these rows.
#'  - Join the baseline flag onto the input dataset.
#'
#' @param raw_dataset Input data frame.
#' @param target_sdtm_variable Name of variable to be derived (`--BLFL` or
#'   `--LOBXFL` where `--` is domain).
#' @param reference_date_variable vector of a date/time from the
#'   Demographics (DM) dataset, which serves as a point of comparison for other
#'   observations in the study. Common choices for this reference variable
#'   include "RFSTDTC" (the date/time of the first study treatment) or
#'   "RFXSTDTC" (the date/time of the first exposure to the study drug).
#' @param baseline_visits A character vector specifying the baseline visits within the study.
#'   These visits are identified as critical points for data collection at the start of the study,
#'   before any intervention is applied. This parameter allows the function to filter and analyze
#'   data specifically from these initial assessment points. For example, baseline visits might
#'   include "Cycle 1 Day 1" if this is the first visit where subjects are assessed prior to receiving treatment.
#' @param baseline_timepoints A character vector of dates in "YYYY-MM-DD" format that specifies
#'   the specific days during the baseline visits when key assessments or measurements were taken.
#'   These timepoints are used to refine the selection of data points to include only those
#'   collected on these specific dates, ensuring that only relevant baseline data is analyzed.
#'   This is particularly important in studies where the timing of measurements can significantly
#'   impact the interpretation of results. An example might be "2020-09-20", indicating a specific
#'   day when baseline data was collected.
#'
#' @return Modified input data frame with baseline flag variable `--BLFL` or
#'   last observation before exposure flag `--LOBXFL` added.
#'
#' @export
#'
#' @examples
#' DM <- read.csv(system.file("derive_blfl/DM.csv", package = "sdtm.oak"))
#' DM
#' raw_dataset <- read.csv(system.file("derive_blfl/raw_dataset.csv", package = "sdtm.oak"))
#' raw_dataset
#' observed_output <- derive_blfl(raw_dataset = raw_dataset,
#'                                DM_dataset = DM,
#'                                target_sdtm_variable = "VSBLFL",
#'                                reference_date_variable = "RFSTDTC")
#' observed_output
derive_blfl <- function(raw_dataset,
                        DM_dataset,
                        target_sdtm_variable,
                        reference_date_variable,
                        baseline_visits = character(),
                        baseline_timepoints = character()) {
  # Check assertions --------------------------------------------------------
  assertion_collection = checkmate::makeAssertCollection()
  # Assert that raw_dataset is a data frame,
  checkmate::assert_data_frame(raw_dataset,
                               col.names = "strict",
                               min.rows = 1,
                               add = assertion_collection)

  # Assert that the input dataset has a "DOMAIN" column
  checkmate::assert_names(names(raw_dataset),
                          must.include = c("DOMAIN", sdtm.oak:::oak_id_vars()),
                          .var.name = "Columns of 'raw_dataset'",
                          add = assertion_collection)

  # Assert DM_dataset is data.frame
  checkmate::assert_data_frame(DM_dataset,
                               col.names = "strict",
                               min.rows = 1,
                               add = assertion_collection)

  # Check if USUBJID and reference_date is present in the DM
  checkmate::assert_names(names(DM),
                          must.include = c("USUBJID", reference_date_variable),
                          .var.name = "Columns of 'DM_dataset'",
                          add = assertion_collection)

  checkmate::assert_character(target_sdtm_variable,
                             min.chars = 1,
                             len = 1,
                             add = assertion_collection)

  checkmate::assert_names(target_sdtm_variable,
                         type = "strict",
                         add = assertion_collection)

  checkmate::assert_character(reference_date_variable,
                              min.chars = 1,
                              len = 1,
                              add = assertion_collection)

  checkmate::assert_names(reference_date_variable,
                          type = "strict",
                          add = assertion_collection)

  checkmate::reportAssertions(assertion_collection)

  # Get domain from input dataset
  domain <- unique(raw_dataset$DOMAIN)
  checkmate::assert_character(domain,
                             min.chars = 1,
                             len = 1,
                             add = assertion_collection)

  # Assert that target_sdtm_variable is a concatenation of domain and "BLFL" or "LOBXFL"
  checkmate::assert_choice(
    target_sdtm_variable,
    choices = c(paste0(domain, "BLFL"),
                paste0(domain, "LOBXFL")),
    add = assertion_collection
  )

  # Determine domain prefixed columns
  suffixes <-
    c("ORRES", "STAT", "TESTCD", "TPT", "DTC", "CAT", "SCAT", "LOC", "LAT",
      "DIR", "METHOD", "SPEC")
  domain_prefixed_names <-
    paste0(domain, suffixes) |>
    setNames(tolower(suffixes))

  # Assert that the input dataset has a "DTC" column
  checkmate::assert_names(names(raw_dataset),
                          must.include = c(domain_prefixed_names[c("orres",
                                                                   "stat",
                                                                   "testcd",
                                                                   "dtc")]),
                          .var.name = "Columns of 'raw_dataset'",
                          add = assertion_collection)
  checkmate::reportAssertions(assertion_collection)

  # End of assertions, work begins ------------------------------------------
  # Create copy of input dataset for modification and processing
  ds_mod <- raw_dataset

  # Filter out rows where --ORRES is missing. Filter out --ORRES in
  # ("ND", "NOT DONE") as well.
  bad_orres_rows <- is.na(ds_mod[[domain_prefixed_names["orres"]]]) |
    trimws(ds_mod[[domain_prefixed_names["orres"]]]) %in% c("ND", "NOT DONE", "")
  ds_mod <- ds_mod[!bad_orres_rows, ]

  # Filter out rows where --STAT is not equal to "NOT DONE"
  ds_mod <- ds_mod |>
    dplyr::filter(
      dplyr::if_any(
        dplyr::any_of(domain_prefixed_names["stat"]), ~ !.x %in% "NOT DONE"))

  if (nrow(ds_mod) == 0) {
    stop(paste0(
      "No rows for which both --ORRES is not missing\n  and --STAT not equals to NOT DONE.\n",
      "  Not able to derive Baseline Flag or Last Observation Before Exposure Flag"
    ))
  }

  # Checking for columns of interest
  con_col <- c(domain_prefixed_names[c("testcd", "dtc", "var_tpt")],
                                     "VISITNUM")

  # Drop those columns from the list which are not present in ds_in
  con_col <- con_col[con_col %in% names(raw_dataset)]

  # Check for any column which is all NA and removing it from con_col list
  h <- which(sapply(ds_mod, function(x) all(is.na(x))))
  if (any(names(h) %in% con_col)) {
    h <- names(h[names(h) %in% con_col])
    # remove all NA columns from con_col
    con_col <- con_col[!con_col %in% h]
  }

  # Keep only USUBJID and reference_date_variable
  DM_dataset <- dplyr::select(DM_dataset,
                              dplyr::all_of(c("USUBJID",
                                              reference_date_variable)))

  # Left join dataset with DM_dataset domain based on USUBJID
  ds_mod <- dplyr::left_join(ds_mod, DM_dataset, by = "USUBJID")

  # Split --DTC and reference_date_variable into date and time parts
  # (partial or missing dates and times set to NA)
  ds_mod$dom_dt <- sdtm.oak:::dtc_datepart(ds_mod[[domain_prefixed_names["dtc"]]])
  ds_mod$dom_tm <- sdtm.oak:::dtc_timepart(ds_mod[[domain_prefixed_names["dtc"]]])
  ds_mod$ref_dt <- sdtm.oak:::dtc_datepart(ds_mod[[reference_date_variable]])
  ds_mod$ref_tm <- sdtm.oak:::dtc_timepart(ds_mod[[reference_date_variable]])


  # If VISIT not in data frame then assign it as "<unspecified>" for processing
  if (!"VISIT" %in% names(ds_mod)) {
    ds_mod[["VISIT"]] <- "<unspecified>"
  }

  # If --TPT not in data frame then assign it as "<unspecified>" for processing
  if (!domain_prefixed_names["tpt"] %in% names(ds_mod)) {
    ds_mod[[domain_prefixed_names["tpt"]]] <- "<unspecified>"
  }

  # Filter on rows that have domain and reference dates not equal to NA
  ds_subset <- dplyr::filter(ds_mod, !is.na(dom_dt) & !is.na(ref_dt))

  # Filter on rows with domain date prior to reference date
  # (*A)
  ds_subset_lt <- dplyr::filter(ds_subset, dom_dt < ref_dt)

  # Filter on rows with domain date equal to reference date but
  # - domain and reference times not equal to NA and
  # - domain time prior to reference time
  # (*B)
  ds_subset_eq_1 <- dplyr::filter(ds_subset, dom_dt == ref_dt,
                                  !is.na(dom_tm) & !is.na(ref_tm),
                                  dom_tm < ref_tm)

  # Filter on rows with domain date equal to reference date but
  # - domain and/or reference time equal to NA and
  # - VISIT is in baseline visits list and
  # - xxTPT is in baseline timepoints list
  # (*C)
  ds_subset_eq_2 <-
    ds_subset |>
    dplyr::filter(dom_dt == ref_dt,
                  is.na(dom_tm) | is.na(ref_tm),
                  (VISIT %in% baseline_visits & get(domain_prefixed_names["tpt"]) %in% baseline_timepoints) |
                  (VISIT %in% baseline_visits & length(baseline_timepoints) == 0) |
                  (get(domain_prefixed_names["tpt"]) %in% baseline_timepoints & length(baseline_visits) == 0))

  # Combine (*A) and (*B) and (*C)
  ds_base <- rbind(ds_subset_lt, ds_subset_eq_1, ds_subset_eq_2)

  # Sort the rows in ascending order with respect to columns from con_col
  ds_base <- dplyr::arrange_at(ds_base, c("USUBJID", con_col))

  if (nrow(ds_base) == 0) {
    message(paste0("There are no baseline records."))
  }

  # Group by USUBJID and --TESTCD and filter on the rows that have max value
  # from --DTC. Keep only the oak id variables, --TESTCD and any key variables
  # from the findings domains (because these are the unique values).
  # Remove any duplicate rows.
  ds_blfl <- ds_base |>
    dplyr::group_by(USUBJID, .data[[domain_prefixed_names["testcd"]]]) |>
    dplyr::slice_max(!!rlang::sym(domain_prefixed_names["dtc"]), na_rm = TRUE) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(c(sdtm.oak:::oak_id_vars(), domain_prefixed_names["testcd"])),
                  dplyr::any_of(
                    c(domain_prefixed_names[c("cat",
                                              "scat",
                                              "spec",
                                              "loc",
                                              "lat",
                                              "dir",
                                              "method")],
                      # For MI domain
                      "MIMRKSTI",
                      "MIGRPID"
                    )
                  )) |>
    dplyr::distinct()

  # Assign the baseline flag variable
  ds_blfl[[target_sdtm_variable]] <- "Y"

  # Join baseline flag onto input dataset
  ds_out <- dplyr::left_join(raw_dataset, ds_blfl, by = sdtm.oak:::oak_id_vars())

  # Assert that merged data frame has same number of rows as input data frame
  if (nrow(ds_out) != nrow(raw_dataset)) {
    stop(sprintf(
      "Internal error: The processed dataset was expected to have the same number of rows (%d) as the input dataset (raw_dataset), but it actually has %d rows.",
      nrow(raw_dataset),
      nrow(ds_out)
    ))
  }

  return(ds_out)
}
