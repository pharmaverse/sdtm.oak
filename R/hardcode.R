#' @importFrom rlang :=
#' @keywords internal
sdtm_hardcode <- function(raw_dat,
                          raw_var,
                          tgt_var,
                          tgt_val,
                          tgt_dat = raw_dat,
                          by = NULL,
                          ct = NULL,
                          cl = NULL) {

  # TODO: Assertions.

  tgt_val <- ct_map(tgt_val, ct = ct, cl = cl)

  # When target dataset and  by variables are provided,
  # the tar_var is added to the input target dataset.
  if((!is.null(by))){
    tgt_dat_out <- raw_dat |>
      #we need to keep only the required variables in the input raw dataset
      dplyr::select(dplyr::all_of(by), rlang::sym(raw_var)) |>
      dplyr::right_join(y = tgt_dat, by = by) |>
      dplyr::mutate("{tgt_var}" := recode(x = !!rlang::sym(raw_var), to = tgt_val)) |>
      dplyr::select(-rlang::sym(raw_var)) |>
      dplyr::relocate(tgt_var, .after = dplyr::last_col())
  } else {
    # When target dataset and  by variables are NOT provided,
    # the tgt_dat_out is created with tar_var & oak_id_vars.
    tgt_dat_out <- raw_dat |>
      dplyr::select(oak_id, raw_source, patient_number,rlang::sym(raw_var)) |>
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
#' @param merge_to_topic_by If `target_dataset` is different than `raw_dataset`,
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
#'   merge_to_topic_by = c("oak_id", "raw_source", "patient_number")
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
                           merge_to_topic_by = NULL) {
  sdtm_hardcode(
    raw_dat = raw_dataset,
    raw_var = raw_variable,
    tgt_var = target_sdtm_variable,
    tgt_val = target_hardcoded_value,
    tgt_dat = target_dataset,
    by = merge_to_topic_by
  )
}

#' @export
#' @rdname harcode
hardcode_ct <- function(raw_dataset,
                        raw_variable,
                        target_sdtm_variable,
                        target_hardcoded_value,
                        target_dataset = raw_dataset,
                        merge_to_topic_by = NULL,
                        study_ct = NULL,
                        target_sdtm_variable_codelist_code = NULL) {
  sdtm_hardcode(
    raw_dat = raw_dataset,
    raw_var = raw_variable,
    tgt_var = target_sdtm_variable,
    tgt_val = target_hardcoded_value,
    tgt_dat = target_dataset,
    by = merge_to_topic_by,
    ct = study_ct,
    cl = target_sdtm_variable_codelist_code
  )
}

