#' Derive an ISO8601 date-time variable
#'
#' [assign_datetime()] maps one or more variables with date/time components in a
#' raw dataset to a target SDTM variable following the ISO8601 format.
#'
#' @param raw_dat The raw dataset (dataframe); must include the
#'   variables passed in `id_vars` and `raw_var`.
#' @param raw_var The raw variable(s): a character vector indicating the name(s)
#'   of the raw variable(s) in `raw_dat` with date or time components to be
#'   parsed into a ISO8601 format variable in `tgt_var`.
#' @param raw_fmt A date/time parsing format. Either a character vector or a
#'   list of character vectors. If a character vector is passed then each
#'   element is taken as parsing format for each variable indicated in
#'   `raw_var`. If a list is provided, then each element must be a character
#'   vector of formats. The first vector of formats is used for parsing the
#'   first variable in `raw_var`, and so on.
#' @param tgt_var The target SDTM variable: a single string indicating the name
#'   of variable to be derived.
#' @param raw_unk A character vector of string literals to be regarded as
#'   missing values during parsing.
#' @param tgt_dat Target dataset: a data frame to be merged against `raw_dat` by
#'   the variables indicated in `id_vars`. This parameter is optional, see
#'   section Value for how the output changes depending on this argument value.
#' @param id_vars Key variables to be used in the join between the raw dataset
#'   (`raw_dat`) and the target data set (`tgt_dat`).
#' @param .warn Whether to warn about parsing failures.
#'
#' @returns The returned data set depends on the value of `tgt_dat`:
#' - If no target dataset is supplied, meaning that `tgt_dat` defaults to
#' `NULL`, then the returned data set is `raw_dat`, selected for the variables
#' indicated in `id_vars`, and a new extra column: the derived variable, as
#' indicated in `tgt_var`.
#' - If the target dataset is provided, then it is merged with the raw data set
#' `raw_dat` by the variables indicated in `id_vars`, with a new column: the
#' derived variable, as indicated in `tgt_var`.
#'
#' @examples
#' # `md1`: an example raw data set.
#' md1 <-
#'   tibble::tribble(
#'     ~oak_id, ~raw_source, ~patient_number, ~MDBDR,        ~MDEDR,        ~MDETM,
#'     1L,      "MD1",       375,             NA,            NA,            NA,
#'     2L,      "MD1",       375,             "15-Sep-20",   NA,            NA,
#'     3L,      "MD1",       376,             "17-Feb-21",   "17-Feb-21",   NA,
#'     4L,      "MD1",       377,             "4-Oct-20",    NA,            NA,
#'     5L,      "MD1",       377,             "20-Jan-20",   "20-Jan-20",   "10:00:00",
#'     6L,      "MD1",       377,             "UN-UNK-2019", "UN-UNK-2019", NA,
#'     7L,      "MD1",       377,             "20-UNK-2019", "20-UNK-2019", NA,
#'     8L,      "MD1",       378,             "UN-UNK-2020", "UN-UNK-2020", NA,
#'     9L,      "MD1",       378,             "26-Jan-20",   "26-Jan-20",   "07:00:00",
#'     10L,     "MD1",       378,             "28-Jan-20",   "1-Feb-20",    NA,
#'     11L,     "MD1",       378,             "12-Feb-20",   "18-Feb-20",   NA,
#'     12L,     "MD1",       379,             "10-UNK-2020", "20-UNK-2020", NA,
#'     13L,     "MD1",       379,             NA,            NA,            NA,
#'     14L,     "MD1",       379,             NA,            "17-Feb-20",   NA
#'   )
#'
#' # Using the raw data set `md1`, derive the variable CMSTDTC from MDBDR using
#' # the parsing format (`raw_fmt`) `"d-m-y"` (day-month-year), while allowing
#' # for the presence of special date component values (e.g. `"UN"` or `"UNK"`),
#' # indicating that these values are missing/unknown (unk).
#' cm1 <-
#'   assign_datetime(
#'     tgt_var = "CMSTDTC",
#'     raw_dat = md1,
#'     raw_var = "MDBDR",
#'     raw_fmt = "d-m-y",
#'     raw_unk = c("UN", "UNK")
#'   )
#'
#' cm1
#'
#' # Inspect parsing failures associated with derivation of CMSTDTC.
#' problems(cm1$CMSTDTC)
#'
#' # `cm_inter`: an example target data set.
#' cm_inter <-
#'   tibble::tibble(
#'     oak_id = 1L:14L,
#'     raw_source = "MD1",
#'     patient_number = c(
#'       375, 375, 376, 377, 377, 377, 377, 378,
#'       378, 378, 378, 379, 379, 379
#'     ),
#'     CMTRT = c(
#'       "BABY ASPIRIN",
#'       "CORTISPORIN",
#'       "ASPIRIN",
#'       "DIPHENHYDRAMINE HCL",
#'       "PARCETEMOL",
#'       "VOMIKIND",
#'       "ZENFLOX OZ",
#'       "AMITRYPTYLINE",
#'       "BENADRYL",
#'       "DIPHENHYDRAMINE HYDROCHLORIDE",
#'       "TETRACYCLINE",
#'       "BENADRYL",
#'       "SOMINEX",
#'       "ZQUILL"
#'     ),
#'     CMINDC = c(
#'       "NA",
#'       "NAUSEA",
#'       "ANEMIA",
#'       "NAUSEA",
#'       "PYREXIA",
#'       "VOMITINGS",
#'       "DIARHHEA",
#'       "COLD",
#'       "FEVER",
#'       "LEG PAIN",
#'       "FEVER",
#'       "COLD",
#'       "COLD",
#'       "PAIN"
#'     )
#'   )
#'
#' # Same derivation as above but now involving the merging with the target
#' # data set `cm_inter`.
#' cm2 <-
#'   assign_datetime(
#'     tgt_dat = cm_inter,
#'     tgt_var = "CMSTDTC",
#'     raw_dat = md1,
#'     raw_var = "MDBDR",
#'     raw_fmt = "d-m-y"
#'   )
#'
#' cm2
#'
#' # Inspect parsing failures associated with derivation of CMSTDTC.
#' problems(cm2$CMSTDTC)
#'
#' # Derive CMSTDTC using both MDEDR and MDETM variables.
#' # Note that the format `"d-m-y"` is used for parsing MDEDR and `"H:M:S"` for
#' # MDETM (correspondence is by positional matching).
#' cm3 <-
#'   assign_datetime(
#'     tgt_var = "CMSTDTC",
#'     raw_dat = md1,
#'     raw_var = c("MDEDR", "MDETM"),
#'     raw_fmt = c("d-m-y", "H:M:S"),
#'     raw_unk = c("UN", "UNK")
#'   )
#'
#' cm3
#'
#' # Inspect parsing failures associated with derivation of CMSTDTC.
#' problems(cm3$CMSTDTC)
#'
#' @export
assign_datetime <-
  function(tgt_dat = NULL,
           tgt_var,
           raw_dat,
           raw_var,
           raw_fmt,
           raw_unk = c("UN", "UNK"),
           id_vars = oak_id_vars(),
           .warn = TRUE) {
    admiraldev::assert_character_vector(raw_var)
    admiraldev::assert_character_scalar(tgt_var)
    admiraldev::assert_character_vector(id_vars)
    assertthat::assert_that(contains_oak_id_vars(id_vars),
      msg = "`id_vars` must include the oak id vars."
    )
    admiraldev::assert_data_frame(raw_dat, required_vars = rlang::syms(c(id_vars, raw_var)))
    admiraldev::assert_data_frame(tgt_dat, required_vars = rlang::syms(id_vars), optional = TRUE)
    admiraldev::assert_character_vector(raw_unk)
    admiraldev::assert_logical_scalar(.warn)

    join_dat <-
      raw_dat |>
      dplyr::select(dplyr::all_of(c(id_vars, raw_var))) |>
      sdtm_join(tgt_dat = tgt_dat, id_vars = id_vars)

    tgt_val <-
      create_iso8601(!!!join_dat[raw_var],
        .format = raw_fmt,
        .na = raw_unk,
        .warn = .warn
      )

    join_dat |>
      mutate("{tgt_var}" := tgt_val) |> # nolint object_name_linter()
      dplyr::select(-dplyr::any_of(setdiff(raw_var, tgt_var))) |>
      dplyr::relocate(dplyr::all_of(tgt_var), .after = dplyr::last_col())
  }
