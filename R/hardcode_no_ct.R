#' Derive an SDTM variable with a hardcoded value
#'
#' [hardcode_no_ct()] maps a hardcoded value to a target SDTM variable that has
#' no terminology restrictions.
#'
#' @param raw_dataset The raw dataset.
#' @param raw_variable The raw variable.
#' @param target_sdtm_variable The target SDTM variable.
#' @param target_hardcoded_value Hardcoded value.
#' @param target_dataset Target dataset. By default the same as `raw_dataset`.
#' @param merge_to_topic_by If `target_dataset` is different than `raw_dataset`,
#'   then this parameter defines keys to use in the join between `raw_dataset`
#'   and `target_dataset`.
#'
#' @examples
#' MD1 <-
#'   tibble::tribble(
#'     ~oak_id, ~raw_source, ~patient_number, ~MDRAW,
#'     1L, "MD1", "PATNUM", "BABY ASPIRIN",
#'     2L, "MD1", "PATNUM", "CORTISPORIN",
#'     3L, "MD1", "PATNUM", NA_character_,
#'     4L, "MD1", "PATNUM", "DIPHENHYDRAMINE HCL"
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
#'     1L, "MD1", "PATNUM", "BABY ASPIRIN", NA,
#'     2L, "MD1", "PATNUM", "CORTISPORIN", "NAUSEA",
#'     3L, "MD1", "PATNUM", "ASPIRIN", "ANEMIA",
#'     4L, "MD1", "PATNUM", "DIPHENHYDRAMINE HCL", "NAUSEA",
#'     5L, "MD1", "PATNUM", "PARACETAMOL", "PYREXIA"
#'   )
#'
#' # Derive a new variable `CMCAT` by overwriting `MDRAW` with the
#' # hardcoded value "GENERAL CONCOMITANT MEDICATIONS" with a prior join to
#' # `target_dataset`.
#'
#' hardcode_no_ct(
#'   raw_dataset = MD1,
#'   raw_variable = "MDRAW",
#'   target_sdtm_variable = "CMCAT",
#'   target_hardcoded_value = "GENERAL CONCOMITANT MEDICATIONS",
#'   target_dataset = CM_INTER,
#'   merge_to_topic_by = c("oak_id", "raw_source", "patient_number")
#' )
#'
#' @importFrom rlang :=
#' @export
hardcode_no_ct <- function(raw_dataset,
                           raw_variable,
                           target_sdtm_variable,
                           target_hardcoded_value,
                           target_dataset = raw_dataset,
                           merge_to_topic_by = NULL) {
  dplyr::right_join(x = raw_dataset, y = target_dataset, by = merge_to_topic_by) |>
    dplyr::mutate("{raw_variable}" := overwrite(!!rlang::sym(raw_variable), target_hardcoded_value)) |>
    dplyr::rename("{target_sdtm_variable}" := raw_variable) |>
    dplyr::relocate(target_sdtm_variable, .after = dplyr::last_col())
}
