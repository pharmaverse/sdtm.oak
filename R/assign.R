#' @importFrom rlang :=
sdtm_assign <- function(raw_dat,
                        raw_var,
                        tgt_var,
                        tgt_dat = NULL,
                        id_vars = oak_id_vars(),
                        ct = NULL,
                        cl = NULL) {

  admiraldev::assert_character_scalar(raw_var)
  admiraldev::assert_character_scalar(tgt_var)
  admiraldev::assert_character_vector(id_vars)
  assertthat::assert_that(contains_oak_id_vars(id_vars),
                          msg = "`id_vars` must include the oak id vars.")
  admiraldev::assert_data_frame(raw_dat, required_vars = rlang::syms(c(id_vars, raw_var)))
  admiraldev::assert_data_frame(tgt_dat, required_vars = rlang::syms(id_vars), optional = TRUE)

  # Recode the raw variable following terminology.
  tgt_val <- ct_map(raw_dat[[raw_var]], ct = ct, cl = cl)

  # Apply derivation by assigning `raw_var` to `tgt_var`.
  # `der_dat`: derived dataset.
  der_dat <-
    raw_dat |>
    dplyr::select(c(id_vars, raw_var)) |>
    dplyr::mutate("{tgt_var}" := tgt_val) |>
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

#' Derive an SDTM variable
#'
#' @description
#' - [assign_no_ct()] maps a variable in a source dataset to a target SDTM
#' variable that has no terminology restrictions.
#'
#' - [assign_ct()] maps a variable in a source dataset to a target SDTM variable
#' following controlled terminology recoding.
#'
#' @param raw_dat The raw dataset.
#' @param raw_var The raw variable.
#' @param tgt_var The target SDTM variable.
#' @param tgt_dat Target dataset.
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
#'   raw_dat = md1,
#'   raw_var = "MDIND",
#'   tgt_var = "CMINDC",
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
#'   raw_dat = md1,
#'   raw_var = "MDIND",
#'   tgt_var = "CMINDC",
#'   tgt_dat = cm_inter,
#'   id_vars = c("oak_id","raw_source","patient_number")
#'   )
#'
#' @name assign
NULL

#' @export
#' @rdname assign
assign_no_ct <- function(raw_dat,
                         raw_var,
                         tgt_var,
                         tgt_dat = NULL,
                         id_vars = oak_id_vars()) {
  sdtm_assign(
    raw_dat = raw_dat,
    raw_var = raw_var,
    tgt_var = tgt_var,
    tgt_dat = tgt_dat,
    id_vars = id_vars
  )
}

#' Derive an SDTM variable with controlled terminology
#'
#' [assign_ct()] maps a variable in a source dataset to a target SDTM variable
#' following controlled terminology recoding.
#'
#' @param raw_dat The raw dataset.
#' @param raw_var The raw variable.
#' @param tgt_var The target SDTM variable.
#' @param tgt_dat Target dataset.
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
#'   raw_dat = md1,
#'   raw_var = "MDRTE",
#'   tgt_var = "CMROUTE",
#'   ct = study_ct,
#'   cl = "C66729"
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
#'   raw_dat = md1,
#'   raw_var = "MDRTE",
#'   tgt_var = "CMROUTE",
#'   tgt_dat = cm_inter,
#'   ct = study_ct,
#'   cl = "C66729"
#'   )
#'
#'
#' @export
#' @rdname assign
assign_ct <- function(raw_dat,
                      raw_var,
                      tgt_var,
                      tgt_dat = NULL,
                      id_vars = oak_id_vars(),
                      ct = NULL,
                      cl = NULL) {
  sdtm_assign(
    raw_dat = raw_dat,
    raw_var = raw_var,
    tgt_var = tgt_var,
    tgt_dat = tgt_dat,
    id_vars = id_vars,
    ct = ct,
    cl = cl
  )
}
