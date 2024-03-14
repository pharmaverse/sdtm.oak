#' @importFrom rlang :=
sdtm_assign <- function(raw_dat,
                        raw_var,
                        tgt_var,
                        tgt_dat,
                        by_var = NULL,
                        ct = NULL,
                        cl = NULL) {

  # TODO: Assertions.

  # When target dataset and  by_var variables are provided,
  # the tar_var is added to the input target dataset.
  # This if block deals when ct is not provided.
  if((!is.null(by_var) && is.null(ct))){
    tgt_dat_out <- raw_dat |>
      dplyr::right_join(y = tgt_dat, by = by_var) |>
      dplyr::mutate("{tgt_var}" := !!rlang::sym(raw_var)) |>
      dplyr::select(-rlang::sym(raw_var)) |>
      dplyr::relocate(tgt_var, .after = dplyr::last_col())
  } else if (is.null(by_var) && is.null(ct)) {
    # When target dataset and  by_var variables are NOT provided,
    # the tgt_dat_out is created with tar_var & oak_id_vars.
    # This if block deals when ct is not provided.
    tgt_dat_out <- raw_dat |>
      dplyr::select(c(oak_id_vars(), raw_var)) |>
      dplyr::mutate("{tgt_var}" := !!rlang::sym(raw_var)) |>
      dplyr::select(-rlang::sym(raw_var))
  } else if (is.null(by_var) && !is.null(ct)) {
    # When target dataset and  by_var variables are NOT provided,
    # the tgt_dat_out is created with tar_var & oak_id_vars.
    # This if block deals when ct is provided.
    tgt_dat_out <- raw_dat |>
      dplyr::select(c(oak_id_vars(), raw_var)) |>
      dplyr::mutate("{tgt_var}" := ct_map(!!rlang::sym(raw_var), ct = ct, cl = cl)) |>
      dplyr::select(-rlang::sym(raw_var)) |>
      dplyr::relocate(tgt_var, .after = dplyr::last_col())
  } else if (!is.null(by_var) && !is.null(ct)) {
    # When target dataset and  by_var variables are provided,
    # the tar_var is added to the input target dataset.
    # This if block deals when ct is provided.
    tgt_dat_out <- raw_dat |>
      dplyr::right_join(y = tgt_dat, by = by_var) |>
      dplyr::mutate("{tgt_var}" := ct_map(!!rlang::sym(raw_var), ct = ct, cl = cl)) |>
      dplyr::select(-rlang::sym(raw_var)) |>
      dplyr::relocate(tgt_var, .after = dplyr::last_col())
  }

  return(tgt_dat_out)

}

#' Derive an SDTM variable
#'
#' @description
#' - [assign_no_ct()] maps a variable in a source dataset to a target SDTM
#' variable that has no terminology restrictions.
#'
#' - [assign_ct()] maps a variable in a source dataset to a target SDTM variable
#' following controlled terminology recoding.
#'
#' @param raw_dataset The raw dataset.
#' @param raw_variable The raw variable.
#' @param target_sdtm_variable The target SDTM variable.
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
#'
#' md1 <-
#'   tibble::tibble(
#'     oak_id = 1:14,
#'     raw_source = "MD1",
#'     patient_number = 101:114,
#'     MDIND = c( "NAUSEA", "NAUSEA", "ANEMIA", "NAUSEA", "PYREXIA",
#'     "VOMITINGS", "DIARHHEA", "COLD",
#'     "FEVER", "LEG PAIN", "FEVER", "COLD", "COLD", "PAIN"
#'     )
#'   )
#'
#' assign_no_ct(
#'   raw_dataset = md1,
#'   raw_variable = "MDIND",
#'   target_sdtm_variable = "CMINDC",
#'   )
#'
#' cm_inter <-
#'   tibble::tibble(
#'     oak_id = 1:14,
#'     raw_source = "MD1",
#'     patient_number = 101:114,
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
#'     CMROUTE = c(
#'       "ORAL",
#'       "ORAL",
#'       NA,
#'       "ORAL",
#'       "ORAL",
#'       "ORAL",
#'       "INTRAMUSCULAR",
#'       "INTRA-ARTERIAL",
#'       NA,
#'       "NON-STANDARD",
#'       "RANDOM_VALUE",
#'       "INTRA-ARTICULAR",
#'       "TRANSDERMAL",
#'       "OPHTHALMIC"
#'     )
#'   )
#'
#' assign_ct(
#'   raw_dataset = md1,
#'   raw_variable = "MDIND",
#'   target_sdtm_variable = "CMINDC",
#'   target_dataset = cm_inter,
#'   merge_to_topic_by = c("oak_id","raw_source","patient_number")
#'   )
#'
#' @name assign
NULL

#' @export
#' @rdname assign
assign_no_ct <- function(raw_dataset,
                         raw_variable,
                         target_sdtm_variable,
                         target_dataset = NULL,
                         merge_to_topic_by = NULL) {
  sdtm_assign(
    raw_dat = raw_dataset,
    raw_var = raw_variable,
    tgt_var = target_sdtm_variable,
    tgt_dat = target_dataset,
    by_var = merge_to_topic_by
  )
}

#' Derive an SDTM variable with controlled terminology
#'
#' [assign_ct()] maps a variable in a source dataset to a target SDTM variable
#' following controlled terminology recoding.
#'
#' @param raw_dataset The raw dataset.
#' @param raw_variable The raw variable.
#' @param target_sdtm_variable The target SDTM variable.
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
#' study_ct <-
#'   tibble::tibble(
#'     codelist_code = rep("C66729", 8L),
#'     term_code = c(
#'       "C28161",
#'       "C38210",
#'       "C38222",
#'       "C38223",
#'       "C38287",
#'       "C38288",
#'       "C38305",
#'       "C38311"
#'     ),
#'     CodedData = c(
#'       "INTRAMUSCULAR",
#'       "EPIDURAL",
#'       "INTRA-ARTERIAL",
#'       "INTRA-ARTICULAR",
#'       "OPHTHALMIC",
#'       "ORAL",
#'       "TRANSDERMAL",
#'       "UNKNOWN"
#'     ),
#'     term_value = CodedData,
#'     collected_value = c(
#'       "IM (Intramuscular)",
#'       "EP (Epidural)",
#'       "IA (Intra-arterial)",
#'       "IJ (Intra-articular)",
#'       "OP (Ophthalmic)",
#'       "PO (Oral)",
#'       "DE (Transdermal)",
#'       "Unknown"
#'     ),
#'     term_preferred_term = c(
#'       "Intramuscular Route of Administration",
#'       "Epidural Route of Administration",
#'       "Intraarterial Route of Administration",
#'       "Intraarticular Route of Administration",
#'       "Ophthalmic Route of Administration",
#'       "Oral Route of Administration",
#'       "Transdermal Route of Administration",
#'       "Unknown Route of Administration"
#'     ),
#'     term_synonyms = c(rep(NA, 5L), "Intraoral Route of Administration; PO", NA, NA),
#'     raw_codelist = rep("ROUTE_CV1", 8L)
#'   )
#'
#' md1 <-
#'   tibble::tibble(
#'     oak_id = 1:14,
#'     raw_source = "MD1",
#'     patient_number = 101:114,
#'     MDRTE = c(
#'       "PO (Oral)",
#'       "PO (Oral)",
#'       NA_character_,
#'       "PO",
#'       "Intraoral Route of Administration",
#'       "PO (Oral)",
#'       "IM (Intramuscular)",
#'       "IA (Intra-arterial)",
#'       "",
#'       "Non-standard",
#'       "random_value",
#'       "IJ (Intra-articular)",
#'       "TRANSDERMAL",
#'       "OPHTHALMIC"
#'     )
#'   )
#'
#' assign_ct(
#'   raw_dataset = md1,
#'   raw_variable = "MDRTE",
#'   study_ct = study_ct,
#'   target_sdtm_variable = "CMROUTE",
#'   target_sdtm_variable_codelist_code = "C66729"
#'   )
#'
#' cm_inter <-
#'   tibble::tibble(
#'     oak_id = 1:14,
#'     raw_source = "MD1",
#'     patient_number = 101:114,
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
#'       NA,
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
#' assign_ct(
#'   raw_dataset = md1,
#'   raw_variable = "MDRTE",
#'   study_ct = study_ct,
#'   target_sdtm_variable = "CMROUTE",
#'   target_sdtm_variable_codelist_code = "C66729",
#'   target_dataset = cm_inter,
#'   merge_to_topic_by = c("oak_id","raw_source","patient_number")
#'   )
#'
#'
#' @export
#' @rdname assign
assign_ct <- function(raw_dataset,
                      raw_variable,
                      target_sdtm_variable,
                      target_dataset = raw_dataset,
                      merge_to_topic_by = NULL,
                      study_ct = NULL,
                      target_sdtm_variable_codelist_code = NULL) {
  sdtm_assign(
    raw_dat = raw_dataset,
    raw_var = raw_variable,
    tgt_var = target_sdtm_variable,
    tgt_dat = target_dataset,
    by_var = merge_to_topic_by,
    ct = study_ct,
    cl = target_sdtm_variable_codelist_code
  )
}
