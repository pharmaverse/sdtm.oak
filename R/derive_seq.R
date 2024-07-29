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
#' according to grouping variables indicated in `rec_vars`.
#'
#' @param tgt_dat The target dataset, a data frame.
#' @param tgt_var The target SDTM variable: a single string indicating the name
#'   of the sequence number (`--SEQ`) variable, e.g. `"DSSEQ"`. Note that
#'   supplying a name not ending in `"SEQ"` will raise a warning.
#' @param rec_vars A character vector of record-level identifier variables.
#' @param sbj_vars A character vector of subject-level identifier variables.
#' @param start_at The sequence numbering starts at this value (default is `1`).
#'
#' @returns Returns the data frame supplied in `tgt_dat` with the newly derived
#'   variable, i.e. the sequence number (`--SEQ`), whose name is that passed in
#'   `tgt_var`. This variable is of type integer.
#'
#' @examples
#' # A VS raw data set example
#' (vs <- read_domain_example("vs"))
#'
#' # Derivation of VSSEQ
#' rec_vars <- c("STUDYID", "USUBJID", "VSTESTCD", "VSDTC", "VSTPTNUM")
#' derive_seq(tgt_dat = vs, tgt_var = "VSSEQ", rec_vars = rec_vars)
#'
#' # An APSC raw data set example
#' (apsc <- read_domain_example("apsc"))
#'
#' # Derivation of APSEQ
#' derive_seq(
#'   tgt_dat = apsc,
#'   tgt_var = "APSEQ",
#'   rec_vars = c("STUDYID", "RSUBJID", "SCTESTCD"),
#'   sbj_vars = c("STUDYID", "RSUBJID")
#' )
#' @export
derive_seq <-
  function(tgt_dat,
           tgt_var,
           rec_vars,
           sbj_vars = sdtm.oak::sbj_vars(),
           start_at = 1L) {
    admiraldev::assert_character_scalar(tgt_var)
    if (!is_seq_name(tgt_var)) {
      cli::cli_warn("Target variable name (`tgt_var`) should end in 'SEQ'.")
    }

    admiraldev::assert_character_vector(rec_vars)
    admiraldev::assert_character_vector(sbj_vars)
    admiraldev::assert_data_frame(tgt_dat,
      required_vars = rlang::syms(rec_vars),
      optional = FALSE
    )

    admiraldev::assert_integer_scalar(start_at, subset = "non-negative")

    tgt_dat |>
      # Ensure that no prior grouping exists that alters ordering and new
      # grouping.
      dplyr::ungroup() |>
      dplyr::arrange(dplyr::across(.cols = dplyr::all_of(rec_vars))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(sbj_vars))) |>
      dplyr::mutate("{tgt_var}" := dplyr::row_number() + start_at - 1L) |> # nolint object_name_linter()
      dplyr::ungroup()
  }

#' Is it a --SEQ variable name
#'
#' [is_seq_name()] returns which variable names end in `"SEQ"`.
#'
#' @param x A character vector.
#'
#' @returns A logical vector.
#'
#' @keywords internal
is_seq_name <- function(x) {
  stringr::str_detect(x, "SEQ$")
}
