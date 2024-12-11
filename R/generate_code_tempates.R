#' The template suffix for the cm code
#'
#' @noRd
#' @keywords internal
cm_template_prefix <- function() {
  stringr::str_glue('
library(sdtm.oak)
library(dplyr)


# Read CT Specification
study_ct <- read.csv("./datasets/sdtm_ct.csv")

# Read in raw data
cm_raw_data <- read.csv("./datasets/cm_raw_data_cdash.csv")

cm_raw_data <- admiral::convert_blanks_to_na(cm_raw_data)

# derive oak_id_vars
cm_raw_data <- cm_raw_data %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "cm_raw_data"
  )

# Read in DM domain to derive study day
dm <- read.csv("./datasets/dm.csv")

dm <- admiral::convert_blanks_to_na(dm)
')
}


#' The template suffix for the cm code
#'
#' @noRd
#' @keywords internal
cm_template_suffix <- function() {
  stringr::str_glue('
dplyr::mutate(
  STUDYID = "test_study",
  DOMAIN = "CM",
  CMCAT = "GENERAL CONMED",
  USUBJID = paste0("test_study", "-", cm_raw_data$PATNUM)
) %>%
derive_seq(tgt_var = "CMSEQ",
           rec_vars= c("USUBJID", "CMTRT")) %>%
derive_study_day(
  sdtm_in = .,
  dm_domain = dm,
  tgdt = "CMENDTC",
  refdt = "RFXSTDTC",
  study_day_var = "CMENDY"
) %>%
derive_study_day(
  sdtm_in = .,
  dm_domain = dm,
  tgdt = "CMSTDTC",
  refdt = "RFXSTDTC",
  study_day_var = "CMSTDY"
) %>%
dplyr::select("STUDYID", "DOMAIN", "USUBJID", "CMSEQ", "CMTRT", "CMCAT", "CMINDC",
              "CMDOSE", "CMDOSTXT", "CMDOSU", "CMDOSFRM", "CMDOSFRQ", "CMROUTE",
              "CMSTDTC", "CMENDTC","CMSTDY", "CMENDY", "CMENRTPT", "CMENTPT")
')
}

#' The template suffix for the vs code
#'
#' @noRd
#' @keywords internal
vs_template_prefix <- function() {
  stringr::str_glue('
library(sdtm.oak)
library(dplyr)


# Read Specification

# Read CT Specification
study_ct <- read.csv("./datasets/sdtm_ct.csv")

# Read in raw data
vitals_raw_data <- read.csv("./datasets/vitals_raw_data.csv",
                   stringsAsFactors = FALSE)

vitals_raw_data <- admiral::convert_blanks_to_na(vitals_raw_data)


# derive oak_id_vars
vitals_raw_data <- vitals_raw_data %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "vitals_raw_data"
  )

# Read in DM domain to derive study day
dm <- read.csv("./datasets/dm.csv")

dm <- admiral::convert_blanks_to_na(dm)
')
}

#' The template suffix for the vs code
#'
#' @noRd
#' @keywords internal
vs_template_suffix <- function() {
  stringr::str_glue('
# Combine all the topic variables into a single data frame. ----
vs_combined <- dplyr::bind_rows(
  vs_asmntdn, vs_sys_bp, vs_dia_bp, vs_pulse, vs_temp,
  vs_resprt, vs_oxy_sat
) %>%
  dplyr::filter(!is.na(.data$VSTESTCD))

# Map qualifiers common to all topic variables ----

vs <- vs_combined %>%
  # Map VSDTC using assign_ct algorithm
  assign_datetime(
    raw_dat = vitals_raw_data,
    raw_var = c("VTLD", "VTLTM"),
    tgt_var = "VSDTC",
    raw_fmt = c(list(c("d-m-y", "dd-mmm-yyyy")), "H:M")
  ) %>%
  # Map VSTPT from TMPTC using assign_ct
  assign_ct(
    raw_dat = vitals_raw_data,
    raw_var = "TMPTC",
    tgt_var = "VSTPT",
    ct_spec = study_ct,
    ct_clst = "TPT",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSTPTNUM from TMPTC using assign_ct
  assign_ct(
    raw_dat = vitals_raw_data,
    raw_var = "TMPTC",
    tgt_var = "VSTPTNUM",
    ct_spec = study_ct,
    ct_clst = "TPTNUM",
    id_vars = oak_id_vars()
  ) %>%
  # Map VISIT from VISIT_NAME using assign_ct
  assign_ct(
    raw_dat = vitals_raw_data,
    raw_var = "VISIT_NAME",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  # Map VISITNUM from VISIT_NAME using assign_ct
  assign_ct(
    raw_dat = vitals_raw_data,
    raw_var = "VISIT_NAME",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  ) %>%
  dplyr::mutate(
    STUDYID = "test_study",
    DOMAIN = "VS",
    VSCAT = "VITAL SIGNS",
    USUBJID = paste0("test_study", "-", .data$patient_number)
  ) %>%
  derive_seq(tgt_var = "VSSEQ",
             rec_vars= c("USUBJID", "VISITNUM", "VSTPTNUM", "VSTESTCD")) %>%
  # A bug in derive_study_day V0.1 that clears the time values in VSDTC
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "VSDTC",
    refdt = "RFXSTDTC",
    study_day_var = "VSDY"
  ) %>%
  dplyr::select("STUDYID", "DOMAIN", "USUBJID", "VSSEQ",
                "VSTESTCD", "VSTEST", "VSCAT", "VSPOS",
                "VSORRES", "VSORRESU", "VSLOC", "VSLAT",
                "VISIT", "VISITNUM", "VSDY", "VSTPT", "VSTPTNUM", "VSDTC" )
')
}
