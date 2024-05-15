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
#' @param tgt_dat The target dataset, a data frame.
#' @param tgt_var The target SDTM variable: a single string indicating the name
#'   of the sequence number (`--SEQ`) variable, e.g. `"DSSEQ"`. Note that
#'   supplying a name not ending in `"SEQ"` will raise a warning.
#' @param id_vars Either a character vector of identifier variables.
#'   Default is the set of variables returned by [oak_id_vars()].
#' @param start_at The sequence numbering starts at this value (default is `1`).
#'
#' @returns Returns the data frame supplied in `tgt_dat` with the newly derived
#'   variable, i.e. the sequence number (`--SEQ`), whose name is that passed in
#'   `tgt_var`. This variable is of type integer.
#'
#' @export
derive_seq <-
  function(tgt_dat,
           tgt_var,
           id_vars,
           start_at = 1L) {
    admiraldev::assert_character_scalar(tgt_var)

    admiraldev::assert_character_vector(id_vars)
    admiraldev::assert_data_frame(tgt_dat,
                                  required_vars = rlang::syms(id_vars),
                                  optional = FALSE)

    admiraldev::assert_integer_scalar(start_at, subset = "non-negative")

    tgt_dat |>
      dplyr::ungroup() |> # ensure that is ungrouped
      dplyr::arrange(dplyr::across(.cols = dplyr::all_of(id_vars))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_vars))) |>
      dplyr::mutate("{tgt_var}" := dplyr::row_number() + start_at - 1L) |>
      dplyr::ungroup()
  }
