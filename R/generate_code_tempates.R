
#' The template suffix for the cm code
#'
#' @noRd
#' @keywords internal
cm_template_prefix <- stringr::str_glue('
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

#' The template suffix for the cm code
#'
#' @noRd
#' @keywords internal
cm_template_suffix <- stringr::str_glue('
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
