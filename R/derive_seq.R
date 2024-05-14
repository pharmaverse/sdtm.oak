#' Derive the sequence number (`--SEQ`) variable
#'
#' @description
#' [derive_seq()] creates a new identifier variable: the sequence number
#' (`--SEQ`).
#'
#' This function adds a newly derived variable to `tgt_dat`, namely the sequence
#' number (`--SEQ`) whose name is `tgt_var`. An integer sequence is generated
#' that, when used in combination with the identifier of the subject, uniquely
#' identifies each record within the domain.
#'
#' If `tgt_dat` pertains a domain that does not contain subject data, then
#' passing `NULL` to `vars_id` will ensure that every row in `tgt_dat` gets a
#' unique integer number in the column named `tgt_var`.
#'
#' @param tgt_dat The target dataset, a data frame.
#' @param tgt_var The target SDTM variable: a single string indicating the name
#'  of the sequence number (`--SEQ`) variable, e.g. `"DSSEQ"`. Note that supplying
#'  a name not ending in `"SEQ"` will raise a warning.
#' @param id_vars Either a character vector of identifier variables or `NULL`.
#'   Default is the set of variables returned by [oak_id_vars()].
#'
#' @returns Returns the data frame supplied in `tgt_dat` with the newly derived
#'   variable, i.e. the sequence number (`--SEQ`) whose name is that passed in
#'   `tgt_var`. This variable is of type integer.
#'
#' @examples
#' #
#'
#'
#' @export
# derive_seq <-
#   function(tgt_dat,
#            tgt_var,
#            id_vars = oak_id_vars(),
#            sort_vars = purrr::set_names(rlang::rep_along(id_vars, "asc"), id_vars),
#            start_at = 1L) {
#
#   return(sort_vars)
#
#   tgt_dat %>%
#     dplyr::arrange(dplyr::across(.cols = dplyr::all_of(id_vars))) |>
#     dplyr::group_by(dplyr::across(dplyr::all_of(id_vars))) |>
#     dplyr::mutate("{tgt_var}" := dplyr::row_number() + start_at - 1L) |>
#     dplyr::ungroup()
# }
