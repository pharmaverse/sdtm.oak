#' @importFrom rlang :=
#' @keywords internal
sdtm_hardcode <- function(raw_dat,
                          raw_var,
                          tgt_var,
                          tgt_val,
                          tgt_dat = NULL,
                          id_vars = oak_id_vars(),
                          ct = NULL,
                          cl = NULL) {

  admiraldev::assert_character_scalar(raw_var)
  admiraldev::assert_character_scalar(tgt_var)
  assertthat::assert_that(assertthat::is.scalar(tgt_val),
                          msg = "`tgt_val` must be a scalar value.")
  admiraldev::assert_character_vector(id_vars)
  assertthat::assert_that(contains_oak_id_vars(id_vars),
                          msg = "`id_vars` must include the oak id vars.")
  admiraldev::assert_data_frame(raw_dat, required_vars = rlang::syms(c(id_vars, raw_var)))
  admiraldev::assert_data_frame(tgt_dat, required_vars = rlang::syms(id_vars), optional = TRUE)

  # Recode the hardcoded value following terminology.
  tgt_val <- ct_map(tgt_val, ct = ct, cl = cl)

  # Apply derivation of the hardcoded value.
  # `der_dat`: derived dataset.
  der_dat <-
    raw_dat |>
    dplyr::select(c(id_vars, raw_var)) |>
    dplyr::mutate("{tgt_var}" := recode(x = !!rlang::sym(raw_var), to = tgt_val)) |>
    dplyr::select(-rlang::sym(raw_var))

  # If a target dataset is supplied, then join the so far derived dataset with
  # the target dataset (`tgt_dat`), otherwise leave it be.
  der_dat <-
    if (!is.null(tgt_dat)) {
      der_dat |>
        dplyr::right_join(y = tgt_dat, by = id_vars) |>
        dplyr::relocate(tgt_var, .after = dplyr::last_col())
    } else {
      der_dat
    }

  der_dat
}

#' Derive an SDTM variable with a hardcoded value
#'
#'
#' @description
#' - [hardcode_no_ct()] maps a hardcoded value to a target SDTM variable that has
#' no terminology restrictions.
#'
#' - [hardcode_ct()] maps a hardcoded value to a target SDTM variable with
#' controlled terminology recoding.
#'
#' @param raw_dat The raw dataset.
#' @param raw_var The raw variable.
#' @param tgt_var The target SDTM variable.
#' @param tgt_val Hardcoded value.
#' @param tgt_dat Target dataset. By default the same as `raw_dataset`.
#' @param id_vars If `target_dataset` is different than `raw_dataset`,
#'   then this parameter defines keys to use in the join between `raw_dataset`
#'   and `target_dataset`.
#' @param ct Study controlled terminology specification.
#' @param cl A codelist code indicating which
#'   subset of the controlled terminology to apply in the derivation.
#'
#' @returns The target dataset with the derived variable `target_sdtm_variable`.
#'
#' @examples
#' MD1 <-
#'   tibble::tribble(
#'     ~oak_id, ~raw_source, ~patient_number, ~MDRAW,
#'     1L, "MD1", 101L, "BABY ASPIRIN",
#'     2L, "MD1", 102L, "CORTISPORIN",
#'     3L, "MD1", 103L, NA_character_,
#'     4L, "MD1", 104L, "DIPHENHYDRAMINE HCL"
#'   )
#'
#' # Derive a new variable `CMCAT` by overwriting `MDRAW` with the
#' # hardcoded value "GENERAL CONCOMITANT MEDICATIONS".
#' hardcode_no_ct(
#'   raw_dat = MD1,
#'   raw_var = "MDRAW",
#'   tgt_var = "CMCAT",
#'   tgt_val = "GENERAL CONCOMITANT MEDICATIONS"
#' )
#'
#' CM_INTER <-
#'   tibble::tribble(
#'     ~oak_id, ~raw_source, ~patient_number, ~CMTRT, ~CMINDC,
#'     1L, "MD1", 101L, "BABY ASPIRIN", NA,
#'     2L, "MD1", 102L, "CORTISPORIN", "NAUSEA",
#'     3L, "MD1", 103L, "ASPIRIN", "ANEMIA",
#'     4L, "MD1", 104L, "DIPHENHYDRAMINE HCL", "NAUSEA",
#'     5L, "MD1", 105L, "PARACETAMOL", "PYREXIA"
#'   )
#'
#' # Derive a new variable `CMCAT` by overwriting `MDRAW` with the
#' # hardcoded value "GENERAL CONCOMITANT MEDICATIONS" with a prior join to
#' # `target_dataset`.
#' hardcode_no_ct(
#'   raw_dat = MD1,
#'   raw_var = "MDRAW",
#'   tgt_var = "CMCAT",
#'   tgt_val = "GENERAL CONCOMITANT MEDICATIONS",
#'   tgt_dat = CM_INTER,
#'   id_vars = c("oak_id", "raw_source", "patient_number")
#' )
#'
#' @name harcode
NULL

#' @export
#' @rdname harcode
hardcode_no_ct <- function(raw_dat,
                           raw_var,
                           tgt_var,
                           tgt_val,
                           tgt_dat = NULL,
                           id_vars = oak_id_vars()) {
  sdtm_hardcode(
    raw_dat = raw_dat,
    raw_var = raw_var,
    tgt_var = tgt_var,
    tgt_val = tgt_val,
    tgt_dat = tgt_dat,
    id_vars = id_vars
  )
}

#' @export
#' @rdname harcode
hardcode_ct <- function(raw_dat,
                        raw_var,
                        tgt_var,
                        tgt_val,
                        tgt_dat = NULL,
                        id_vars = oak_id_vars(),
                        ct,
                        cl) {
  sdtm_hardcode(
    raw_dat = raw_dat,
    raw_var = raw_var,
    tgt_var = tgt_var,
    tgt_val = tgt_val,
    tgt_dat = tgt_dat,
    id_vars = id_vars,
    ct = ct,
    cl = cl
  )
}

