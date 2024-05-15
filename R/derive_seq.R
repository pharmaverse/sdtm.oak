#' Derive the sequence number (`--SEQ`) variable
#'
#' @description
#' [derive_seq()] creates a new identifier variable: the sequence number
#' (`--SEQ`).
#'
#' This function adds a newly derived variable to `tgt_dat`, namely the sequence
#' number (`--SEQ`) whose name is the one provided in `tgt_var`. An integer
#' sequence is generated that uniquely identifies each record within the domain.
#'
#' Prior to the derivation of `tgt_var`, the data frame `tgt_dat` is sorted
#' according to grouping variables indicated in `id_vars`.
#'
#' Passing `NULL` to `id_vars` will ensure that every row in `tgt_dat` gets a
#' unique integer number in the column named by `tgt_var`.
#'
#' @param tgt_dat The target dataset, a data frame.
#' @param tgt_var The target SDTM variable: a single string indicating the name
#'   of the sequence number (`--SEQ`) variable, e.g. `"DSSEQ"`. Note that
#'   supplying a name not ending in `"SEQ"` will raise a warning.
#' @param id_vars Either a character vector of identifier variables or `NULL`.
#'   Default is the set of variables returned by [oak_id_vars()].
#' @param start_at The sequence numbering starts at this value (default is `1`).
#'
#' @returns Returns the data frame supplied in `tgt_dat` with the newly derived
#'   variable, i.e. the sequence number (`--SEQ`), whose name is that passed in
#'   `tgt_var`. This variable is of type integer.
#'
#' @examples
#' # An example Medical Devices (MD) domain raw data set.
#' md <-
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
#' # Derive the sequence number MDSEQ. By default, the grouping variables
#' # (`id_vars`) are the ones defined by `oak_id_vars()`.
#' derive_seq(tgt_dat = md, tgt_var = "MDSEQ")
#'
#' # An example Vital Signs (VS) domain raw data set.
#' vs <- tibble::tribble(
#'   ~oak_id,   ~raw_source, ~patient_number, ~VSTESTCD, ~VISITNUM, ~VSTPTNUM,
#'   "PILOT01", "VS",        703,             "DIABP",   3,         815,
#'   "PILOT01", "VS",        703,             "DIABP",   3,         816,
#'   "PILOT01", "VS",        703,             "SYSBP",   4,         816,
#'   "PILOT01", "VS",        716,             "DIABP",   3,         815,
#'   "PILOT01", "VS",        716,             "DIABP",   3,         816,
#'   "PILOT01", "VS",        716,             "DIABP",   4,         815,
#'   "PILOT01", "VS",        716,             "SYSBP",   3,         815,
#'   "PILOT01", "VS",        716,             "SYSBP",   4,         816
#' )
#'
#' # Derive sequence number by explicitly indicating the records' grouping
#' # defined by the variables `patient_number` and `VSTESTCD`.
#' vs_id_vars <- c("patient_number", "VSTESTCD")
#' derive_seq(tgt_dat = vs, tgt_var = "ASEQ", id_vars = vs_id_vars)
#'
#' # If no grouping variables are provided then the rows are numbered
#' # sequentially in the order provided.
#' derive_seq(tgt_dat = vs, tgt_var = "ASEQ", id_vars = NULL)
#'
#' @export
derive_seq <-
  function(tgt_dat,
           tgt_var,
           id_vars = oak_id_vars(),
           start_at = 1L) {

    admiraldev::assert_character_scalar(tgt_var)

    if (is.null(id_vars)) {
      admiraldev::assert_data_frame(tgt_dat, optional = FALSE)

    } else {
      admiraldev::assert_character_vector(id_vars)
      admiraldev::assert_data_frame(tgt_dat,
                                    required_vars = rlang::syms(id_vars),
                                    optional = FALSE)
    }

    admiraldev::assert_integer_scalar(start_at, subset = "non-negative")

    if (is.null(id_vars)) {
      tgt_dat |>
        dplyr::ungroup() |> # ensure that is ungrouped
        dplyr::mutate("{tgt_var}" := dplyr::row_number() + start_at - 1L)
    } else {
      tgt_dat |>
        dplyr::ungroup() |> # ensure that is ungrouped
        dplyr::arrange(dplyr::across(.cols = dplyr::all_of(id_vars))) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(id_vars))) |>
        dplyr::mutate("{tgt_var}" := dplyr::row_number() + start_at - 1L) |>
        dplyr::ungroup()
    }
  }
