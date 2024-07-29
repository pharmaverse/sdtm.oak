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
#' @keywords internal
dtc_datepart <- function(dtc, partial_as_na = TRUE) {
  # Assert that dtc is a character vector
  admiraldev::assert_character_vector(dtc)

  # Extract date part from ISO 8601 date/time variable
  dt <- sub("^([^T]+).*", "\\1", dtc)

  # Set partial or missing dates to NA, depending on partial_as_na parameter
  if (partial_as_na) {
    dt <- ifelse(nchar(dt) < 10L, NA, dt)
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
#' @keywords internal
dtc_timepart <- function(dtc, partial_as_na = TRUE, ignore_seconds = TRUE) {
  # Assert that dtc is a character vector
  admiraldev::assert_character_vector(dtc)

  # Determine length of time part depending on ignore_seconds parameter
  tm_length <- ifelse(ignore_seconds, 5L, 8L)

  # Extract time part from ISO 8601 date/time variable
  tm <- substr(sub("^([^T]+)T?", "", dtc), 1L, tm_length)

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
#'
#' The derivation is as follows:
#'
#'  - Remove records where the result (`--ORRES`) is missing. Also, exclude records
#'    with results labeled as "ND" (No Data) or "NOT DONE" in the `--ORRES` column,
#'    which indicate that the measurement or observation was not completed.
#'  - Remove records where the status (`--STAT`) indicates the observation or test
#'    was not performed, marked as "NOT DONE".
#'  - Divide the date and time column (`--DTC`) and the reference date/time
#'    variable (`ref_var`) into separate date and time components. Ignore
#'    any seconds recorded in the time component, focusing only on hours and
#'    minutes for further calculations.
#'  - Set partial or missing dates to `NA`.
#'  - Set partial or missing times to `NA`.
#'  - Filter on rows that have domain and reference dates not equal to
#'    `NA`. (Ref to as **X**)
#'  - Filter **X** on rows with domain date (--DTC) prior to (less than)
#'    reference date. (Ref to as **A**)
#'  - Filter **X** on rows with domain date (--DTC) equal to reference date but
#'    domain and reference times not equal to `NA` and domain time prior to (less
#'    than) reference time. (Ref to as **B**)
#'  - Filter **X** on rows with domain date (--DTC) equal to reference date but
#'    domain and/or reference time equal to NA and:
#'      - VISIT is in baseline visits list (if it exists) and
#'      - xxTPT is in baseline timepoints list (if it exists).
#'    (Ref to as **C**)
#'  - Combine the rows from **A**, **B**, and **C** to get a
#' data frame of pre-reference date observations. Sort the rows by `USUBJID`,
#' `--STAT`, and `--ORRES`.
#'  - Group by `USUBJID` and `--TESTCD` and filter on the rows
#' that have maximum value from `--DTC`. Keep only the oak id variables and
#' `--TESTCD` (because these are the unique values). Remove any duplicate rows.
#' Assign the baseline flag variable, `--BLFL`, the last observation before
#' exposure flag (`--LOBXFL`) variable to these rows.
#'  - Join the baseline flag onto the input dataset based on oak id vars
#'
#' @param sdtm_in Input SDTM domain.
#' @param dm_domain DM domain with the reference variable `ref_var`
#' @param tgt_var Name of variable to be derived (`--BLFL` or
#'   `--LOBXFL` where `--` is domain).
#' @param ref_var vector of a date/time from the
#'   Demographics (DM) dataset, which serves as a point of comparison for other
#'   observations in the study. Common choices for this reference variable
#'   include "RFSTDTC" (the date/time of the first study treatment) or
#'   "RFXSTDTC" (the date/time of the first exposure to the study drug).
#' @param baseline_visits A character vector specifying the baseline visits within the study.
#'   These visits are identified as critical points for data collection at the start of the study,
#'   before any intervention is applied.  This allows the function to assign the baseline
#'   flag if the --DTC matches to the reference date.
#' @param baseline_timepoints A character vector of timepoints values in --TPT that specifies
#'   the specific timepoints during the baseline visits when key assessments or measurements were taken.
#'   This allows the function to assign the baseline flag if the --DTC matches to the reference date.
#'
#' @return Modified input data frame with baseline flag variable `--BLFL` or
#'   last observation before exposure flag `--LOBXFL` added.
#'
#' @export
#'
#' @examples
#' dm <- tibble::tribble(
#'   ~USUBJID, ~RFSTDTC, ~RFXSTDTC,
#'   "test_study-375", "2020-09-28T10:10", "2020-09-28T10:10",
#'   "test_study-376", "2020-09-21T11:00", "2020-09-21T11:00",
#'   "test_study-377", NA, NA,
#'   "test_study-378", "2020-01-20T10:00", "2020-01-20T10:00",
#'   "test_study-379", NA, NA,
#' )
#'
#' dm
#'
#' sdtm_in <-
#'   tibble::tribble(
#'     ~DOMAIN,
#'     ~oak_id,
#'     ~raw_source,
#'     ~patient_number,
#'     ~USUBJID,
#'     ~VSDTC,
#'     ~VSTESTCD,
#'     ~VSORRES,
#'     ~VSSTAT,
#'     ~VISIT,
#'     "VS",
#'     1L,
#'     "VTLS1",
#'     375L,
#'     "test_study-375",
#'     "2020-09-01T13:31",
#'     "DIABP",
#'     "90",
#'     NA,
#'     "SCREENING",
#'     "VS",
#'     2L,
#'     "VTLS1",
#'     375L,
#'     "test_study-375",
#'     "2020-10-01T11:20",
#'     "DIABP",
#'     "90",
#'     NA,
#'     "SCREENING",
#'     "VS",
#'     1L,
#'     "VTLS1",
#'     375L,
#'     "test_study-375",
#'     "2020-09-28T10:10",
#'     "PULSE",
#'     "ND",
#'     NA,
#'     "SCREENING",
#'     "VS",
#'     2L,
#'     "VTLS1",
#'     375L,
#'     "test_study-375",
#'     "2020-10-01T13:31",
#'     "PULSE",
#'     "85",
#'     NA,
#'     "SCREENING",
#'     "VS",
#'     1L,
#'     "VTLS2",
#'     375L,
#'     "test_study-375",
#'     "2020-09-28T10:10",
#'     "SYSBP",
#'     "120",
#'     NA,
#'     "SCREENING",
#'     "VS",
#'     2L,
#'     "VTLS2",
#'     375L,
#'     "test_study-375",
#'     "2020-09-28T10:05",
#'     "SYSBP",
#'     "120",
#'     NA,
#'     "SCREENING",
#'     "VS",
#'     1L,
#'     "VTLS1",
#'     376L,
#'     "test_study-376",
#'     "2020-09-20",
#'     "DIABP",
#'     "75",
#'     NA,
#'     "SCREENING",
#'     "VS",
#'     1L,
#'     "VTLS1",
#'     376L,
#'     "test_study-376",
#'     "2020-09-20",
#'     "PULSE",
#'     NA,
#'     "NOT DONE",
#'     "SCREENING",
#'     "VS",
#'     2L,
#'     "VTLS1",
#'     376L,
#'     "test_study-376",
#'     "2020-09-20",
#'     "PULSE",
#'     "110",
#'     NA,
#'     "SCREENING",
#'     "VS",
#'     2L,
#'     "VTLS1",
#'     378L,
#'     "test_study-378",
#'     "2020-01-20T10:00",
#'     "PULSE",
#'     "110",
#'     NA,
#'     "SCREENING",
#'     "VS",
#'     3L,
#'     "VTLS1",
#'     378L,
#'     "test_study-378",
#'     "2020-01-21T11:00",
#'     "PULSE",
#'     "105",
#'     NA,
#'     "SCREENING"
#'   )
#'
#' sdtm_in
#'
#' observed_output <- derive_blfl(
#'   sdtm_in = sdtm_in,
#'   dm_domain = dm,
#'   tgt_var = "VSLOBXFL",
#'   ref_var = "RFXSTDTC",
#'   baseline_visits = c("SCREENING")
#' )
#' observed_output
#'
derive_blfl <- function(sdtm_in,
                        dm_domain,
                        tgt_var,
                        ref_var,
                        baseline_visits = character(),
                        baseline_timepoints = character()) {
  # Check assertions --------------------------------------------------------
  # Check variables are character scalars
  admiraldev::assert_character_scalar(tgt_var)
  admiraldev::assert_character_scalar(ref_var)

  # Assert that sdtm_in is a data frame, contains DOMAIN and oak id vars
  admiraldev::assert_data_frame(
    sdtm_in,
    required_vars = rlang::syms(c("DOMAIN", oak_id_vars()))
  )

  # Assert dm_domain is data.frame
  admiraldev::assert_data_frame(
    dm_domain,
    required_vars = rlang::syms(c("USUBJID", ref_var))
  )

  # Get domain from input dataset
  domain <- unique(sdtm_in$DOMAIN)

  admiraldev::assert_character_scalar(domain)

  # Assert that tgt_var is a concatenation of domain and "BLFL" or "LOBXFL"
  admiraldev::assert_character_scalar(tgt_var,
    values = c(
      paste0(domain, "BLFL"),
      paste0(domain, "LOBXFL")
    )
  )

  # Determine domain prefixed columns
  suffixes <-
    c(
      "ORRES", "STAT", "TESTCD", "TPT", "DTC", "CAT", "SCAT", "LOC", "LAT",
      "DIR", "METHOD", "SPEC"
    )
  domain_prefixed_names <-
    paste0(domain, suffixes) |>
    stats::setNames(tolower(suffixes))

  # Assert that the input dataset has a "DTC" column
  admiraldev::assert_data_frame(
    sdtm_in,
    required_vars = rlang::syms(c(domain_prefixed_names[c(
      "orres",
      "stat",
      "testcd",
      "dtc"
    )]))
  )

  # End of assertions, work begins ------------------------------------------
  # Create copy of input dataset for modification and processing
  ds_mod <- sdtm_in

  # Filter out rows where --ORRES is missing. Filter out --ORRES in
  # ("ND", "NOT DONE") as well.
  bad_orres_rows <- is.na(ds_mod[[domain_prefixed_names["orres"]]]) |
    trimws(ds_mod[[domain_prefixed_names["orres"]]]) %in% c("ND", "NOT DONE", "")
  ds_mod <- ds_mod[!bad_orres_rows, ]

  # Filter out rows where --STAT is not equal to "NOT DONE"
  ds_mod <-
    ds_mod |>
    dplyr::filter(dplyr::if_any(
      dplyr::any_of(domain_prefixed_names["stat"]),
      ~ !.x %in% "NOT DONE"
    ))

  if (nrow(ds_mod) == 0L) {
    cli::cli_abort(paste0(
      "No rows for which both --ORRES is not missing\n  and --STAT not equals to NOT DONE.\n",
      "  Not able to derive Baseline Flag or Last Observation Before Exposure Flag"
    ))
  }

  # Checking for columns of interest
  con_col <- c(domain_prefixed_names[c("testcd", "dtc", "var_tpt")], "VISIT")

  # Drop those columns from the list which are not present in ds_in
  con_col <- con_col[con_col %in% names(sdtm_in)]

  # Check for any column which is all NA and removing it from con_col list
  h <- which(sapply(ds_mod, function(x) all(is.na(x))))
  if (any(names(h) %in% con_col)) {
    h <- names(h[names(h) %in% con_col])
    # remove all NA columns from con_col
    con_col <- con_col[!con_col %in% h]
  }

  # Keep only USUBJID and ref_var
  dm_domain <- dplyr::select(dm_domain, dplyr::all_of(c("USUBJID", ref_var)))

  # Left join dataset with dm_domain domain based on USUBJID
  ds_mod <- dplyr::left_join(ds_mod, dm_domain, by = "USUBJID")

  # Split --DTC and ref_var into date and time parts
  # (partial or missing dates and times set to NA)
  ds_mod$dom_dt <- dtc_datepart(ds_mod[[domain_prefixed_names["dtc"]]])
  ds_mod$dom_tm <- dtc_timepart(ds_mod[[domain_prefixed_names["dtc"]]])
  ds_mod$ref_dt <- dtc_datepart(ds_mod[[ref_var]])
  ds_mod$ref_tm <- dtc_timepart(ds_mod[[ref_var]])


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
  ds_subset_eq_1 <- dplyr::filter(
    ds_subset, dom_dt == ref_dt,
    !is.na(dom_tm) & !is.na(ref_tm),
    dom_tm < ref_tm
  )

  # Filter on rows with domain date equal to reference date but
  # - domain and/or reference time equal to NA and
  # - VISIT is in baseline visits list and
  # - xxTPT is in baseline timepoints list
  # (*C)
  ds_subset_eq_2 <-
    ds_subset |>
    dplyr::filter(
      dom_dt == ref_dt,
      is.na(dom_tm) | is.na(ref_tm) | dom_tm == ref_tm,
      (VISIT %in% baseline_visits & get(domain_prefixed_names["tpt"]) %in% baseline_timepoints) |
        (VISIT %in% baseline_visits & length(baseline_timepoints) == 0L) |
        (get(domain_prefixed_names["tpt"]) %in% baseline_timepoints & length(baseline_visits) == 0L)
    )

  # Combine (*A) and (*B) and (*C)
  ds_base <- rbind(ds_subset_lt, ds_subset_eq_1, ds_subset_eq_2)

  # Sort the rows in ascending order with respect to columns from con_col
  ds_base <- dplyr::arrange_at(ds_base, c("USUBJID", con_col))

  if (nrow(ds_base) == 0L) {
    cli::cli_inform("There are no baseline records.")
  }

  # Group by USUBJID and --TESTCD and filter on the rows that have max value
  # from --DTC. Keep only the oak id variables, --TESTCD and any key variables
  # from the findings domains (because these are the unique values).
  # Remove any duplicate rows.
  ds_blfl <- ds_base |>
    dplyr::group_by(USUBJID, .data[[domain_prefixed_names["testcd"]]]) |>
    dplyr::slice_max(!!rlang::sym(domain_prefixed_names["dtc"]), na_rm = TRUE) |>
    dplyr::ungroup() |>
    dplyr::select(
      dplyr::all_of(c(sdtm.oak::oak_id_vars(), domain_prefixed_names[["testcd"]])),
      dplyr::any_of(
        c(domain_prefixed_names[c(
          "cat",
          "scat",
          "spec",
          "loc",
          "lat",
          "dir",
          "method"
        )])
      )
    ) |>
    dplyr::distinct()

  # Assign the baseline flag variable
  ds_blfl[[tgt_var]] <- "Y"

  # Join baseline flag onto input dataset
  ds_out <- dplyr::left_join(sdtm_in, ds_blfl, by = c(
    domain_prefixed_names[["testcd"]],
    sdtm.oak::oak_id_vars()
  ))

  # Assert that merged data frame has same number of rows as input data frame
  if (nrow(ds_out) != nrow(sdtm_in)) {
    cli::cli_abort(
      "Internal error: The processed dataset was expected to have the same
      number of rows ({nrow(sdtm_in)}) as the input dataset (sdtm_in), but it actually has {nrow(ds_out)} rows."
    )
  }

  return(ds_out)
}
