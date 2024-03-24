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

  # TODO: Assertions. assert that id_vars always contains "oak_id",
  # "raw_source", "patient_number"

  tgt_val <- ct_map(tgt_val, ct = ct, cl = cl)

  # When target dataset and id_vars variables are provided, the tar_var is added
  # to the input target dataset.
  tgt_dat_out <-
    if (!is.null(tgt_dat)) {
      raw_dat |>
        #we need to keep only the required variables in the input raw dataset
        dplyr::select(c(id_vars, raw_var)) |>
        dplyr::right_join(y = tgt_dat, by = id_vars) |>
        dplyr::mutate("{tgt_var}" := recode(x = !!rlang::sym(raw_var), to = tgt_val)) |>
        dplyr::select(-rlang::sym(raw_var)) |>
        dplyr::relocate(tgt_var, .after = dplyr::last_col())
    } else {
      # When target dataset and  id_vars variables are NOT provided, the
      # tgt_dat_out is created with tar_var & oak_id_vars.
      raw_dat |>
        dplyr::select(c(id_vars, raw_var)) |>
        dplyr::mutate("{tgt_var}" := recode(x = !!rlang::sym(raw_var), to = tgt_val)) |>
        dplyr::select(-rlang::sym(raw_var))
    }

  return(tgt_dat_out)

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
#' @param raw_dataset The raw dataset.
#' @param raw_variable The raw variable.
#' @param target_sdtm_variable The target SDTM variable.
#' @param target_hardcoded_value Hardcoded value.
#' @param target_dataset Target dataset. By default the same as `raw_dataset`.
#' @param id_vars If `target_dataset` is different than `raw_dataset`,
#'   then this parameter defines keys to use in the join between `raw_dataset`
#'   and `target_dataset`.
#' @param study_ct Study controlled terminology specification.
#' @param target_sdtm_variable_codelist_code A codelist code indicating which
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
#'   raw_dataset = MD1,
#'   raw_variable = "MDRAW",
#'   target_sdtm_variable = "CMCAT",
#'   target_hardcoded_value = "GENERAL CONCOMITANT MEDICATIONS"
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
#'   raw_dataset = MD1,
#'   raw_variable = "MDRAW",
#'   target_sdtm_variable = "CMCAT",
#'   target_hardcoded_value = "GENERAL CONCOMITANT MEDICATIONS",
#'   target_dataset = CM_INTER,
#'   id_vars = c("oak_id", "raw_source", "patient_number")
#' )
#'
#' @name harcode
NULL

#' @export
#' @rdname harcode
hardcode_no_ct <- function(raw_dataset,
                           raw_variable,
                           target_sdtm_variable,
                           target_hardcoded_value,
                           target_dataset = NULL,
                           id_vars = NULL) {
  sdtm_hardcode(
    raw_dat = raw_dataset,
    raw_var = raw_variable,
    tgt_var = target_sdtm_variable,
    tgt_val = target_hardcoded_value,
    tgt_dat = target_dataset,
    id_vars = id_vars
  )
}

#' @export
#' @rdname harcode
hardcode_ct <- function(raw_dataset,
                        raw_variable,
                        target_sdtm_variable,
                        target_hardcoded_value,
                        target_dataset = NULL,
                        id_vars = NULL,
                        study_ct,
                        target_sdtm_variable_codelist_code) {
  sdtm_hardcode(
    raw_dat = raw_dataset,
    raw_var = raw_variable,
    tgt_var = target_sdtm_variable,
    tgt_val = target_hardcoded_value,
    tgt_dat = target_dataset,
    id_vars = id_vars,
    ct = study_ct,
    cl = target_sdtm_variable_codelist_code
  )
}

