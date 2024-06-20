# Name: CM domain
#
# Label: R program to create CM Domain
#
# Input raw data: cm_raw
# study_controlled_terminology : study_ct
#
#

library(sdtm.oak)
library(dplyr)


# Read Specification

study_ct <- read.csv(system.file("raw_data/sdtm_ct.csv",
  package = "sdtm.oak"
))

# Read in raw data

cm_raw <- read.csv(system.file("raw_data/cm_raw_data.csv",
  package = "sdtm.oak"
)) %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "cm_raw"
  )

dm <- read.csv(system.file("raw_data/dm.csv",
  package = "sdtm.oak"
))

# Create CM domain. The first step in creating CM domain is to create the topic variable

cm <-
  # Derive topic variable
  assign_no_ct(
    raw_dat = cm_raw,
    raw_var = "MDRAW",
    tgt_var = "CMTRT"
  ) %>%
  # Derive CMGRPID
  assign_no_ct(
    raw_dat = cm_raw,
    raw_var = "MDNUM",
    tgt_var = "CMGRPID",
    id_vars = oak_id_vars()
  ) %>%
  # DERIVE CMINDC
  assign_no_ct(
    raw_dat = cm_raw,
    raw_var = "MDIND",
    tgt_var = "CMINDC",
    id_vars = oak_id_vars()
  ) %>%
  # Derive CMSTDTC. This function calls create_iso8601
  assign_datetime(
    raw_dat = cm_raw,
    raw_var = c("MDBDR", "MDBTM"),
    tgt_var = "CMSTDTC",
    raw_fmt = c(list(c("d-m-y", "dd mmm yyyy")), "H:M"),
    raw_unk = c("UN", "UNK")
  ) %>%
  # Derive qualifier CMSTRTPT  Annotation text is If MDPRIOR == 1 then CM.CMSTRTPT = 'BEFORE'
  hardcode_ct(
    raw_dat = condition_add(cm_raw, MDPRIOR == "1"),
    raw_var = "MDPRIOR",
    tgt_var = "CMSTRTPT",
    tgt_val = "BEFORE",
    ct_spec = study_ct,
    ct_clst = "C66728",
    id_vars = oak_id_vars()
  ) %>%
  # Derive qualifier CMSTTPT  Annotation text is If MDPRIOR == 1 then CM.CMSTTPT = 'SCREENING'
  hardcode_no_ct(
    raw_dat = condition_add(cm_raw, MDPRIOR == "1"),
    raw_var = "MDPRIOR",
    tgt_var = "CMSTTPT",
    tgt_val = "SCREENING",
    id_vars = oak_id_vars()
  ) %>%
  # Derive CMENDTC. This function calls create_iso8601
  assign_datetime(
    raw_dat = cm_raw,
    raw_var = c("MDEDR", "MDETM"),
    tgt_var = "CMENDTC",
    raw_fmt = c("d-m-y", "H:M"),
    raw_unk = c("UN", "UNK")
  ) %>%
  # Derive qualifier CMENRTPT  Annotation text is If MDONG == 1 then CM.CMENRTPT = 'ONGOING'
  hardcode_ct(
    raw_dat = condition_add(cm_raw, MDONG == "1"),
    raw_var = "MDONG",
    tgt_var = "CMENRTPT",
    tgt_val = "ONGOING",
    ct_spec = study_ct,
    ct_clst = "C66728",
    id_vars = oak_id_vars()
  ) %>%
  # Derive qualifier CMENTPT  Annotation text is If MDONG == 1 then CM.CMENTPT = 'DATE OF LAST ASSESSMENT'
  hardcode_no_ct(
    raw_dat = condition_add(cm_raw, MDONG == "1"),
    raw_var = "MDONG",
    tgt_var = "CMENTPT",
    tgt_val = "DATE OF LAST ASSESSMENT",
    id_vars = oak_id_vars()
  ) %>%
  # Derive qualifier CMDOS If collected value in raw_var DOS is numeric then CM.CMDOSE
  assign_no_ct(
    raw_dat = condition_add(cm_raw, is.numeric(DOS)),
    raw_var = "DOS",
    tgt_var = "CMDOS",
    id_vars = oak_id_vars()
  ) %>%
  # Derive qualifier CMDOS If collected value in raw_var DOS is character then CM.CMDOSTXT
  assign_no_ct(
    raw_dat = condition_add(cm_raw, is.character(DOS)),
    raw_var = "DOS",
    tgt_var = "CMDOSTXT",
    id_vars = oak_id_vars()
  ) %>%
  # Derive qualifier CMDOSU
  assign_ct(
    raw_dat = cm_raw,
    raw_var = "DOSU",
    tgt_var = "CMDOSU",
    ct_spec = study_ct,
    ct_clst = "C71620",
    id_vars = oak_id_vars()
  ) %>%
  # Derive qualifier CMDOSFRM
  assign_ct(
    raw_dat = cm_raw,
    raw_var = "MDFORM",
    tgt_var = "CMDOSFRM",
    ct_spec = study_ct,
    ct_clst = "C66726",
    id_vars = oak_id_vars()
  ) %>%
  # DERIVE CMROUTE
  assign_ct(
    raw_dat = cm_raw,
    raw_var = "MDRTE",
    tgt_var = "CMROUTE",
    ct_spec = study_ct,
    ct_clst = "C66729",
    id_vars = oak_id_vars()
  ) %>%
  # DERIVE CMDOSFRQ
  assign_ct(
    raw_dat = cm_raw,
    raw_var = "MDFRQ",
    tgt_var = "CMDOSFRQ",
    ct_spec = study_ct,
    ct_clst = "C71113",
    id_vars = oak_id_vars()
  ) %>%
  # Derive qualifier CMPROPH  Annotation text is If MDPROPH == 1 then CM.CMPROPH = 'Y'
  hardcode_ct(
    raw_dat = condition_add(cm_raw, MDPROPH == "1"),
    raw_var = "MDPROPH",
    tgt_var = "CMPROPH",
    tgt_val = "Y",
    ct_spec = study_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  # Derive qualifier CMMODIFY  Annotation text  If collected value in MODIFY
  # in cm_raw is different to CM.CMTRT then
  # assign the collected value to CMMODIFY in CM domain (CM.CMMODIFY)
  {
    assign_no_ct(
      raw_dat = cm_raw,
      raw_var = "MODIFY",
      tgt_dat = condition_add(., MODIFY != CMTRT, .dat2 = cm_raw),
      tgt_var = "CMMODIFY",
      id_vars = oak_id_vars()
    )
  } %>%
  # Derive CMDRG
  assign_no_ct(
    raw_dat = cm_raw,
    raw_var = "CMDRG",
    tgt_var = "CMDRG",
    id_vars = oak_id_vars()
  ) %>%
  # Derive CMDRGCD
  assign_no_ct(
    raw_dat = cm_raw,
    raw_var = "CMDRGCD",
    tgt_var = "CMDRGCD",
    id_vars = oak_id_vars()
  ) %>%
  # Derive CMDECOD
  assign_no_ct(
    raw_dat = cm_raw,
    raw_var = "CMDECOD",
    tgt_var = "CMDECOD",
    id_vars = oak_id_vars()
  ) %>%
  # Derive CMPNCD
  assign_no_ct(
    raw_dat = cm_raw,
    raw_var = "CMPNCD",
    tgt_var = "CMPNCD",
    id_vars = oak_id_vars()
  ) %>%
  dplyr::mutate(
    STUDYID = "test_study",
    DOMAIN = "CM",
    CMCAT = "GENERAL CONMED",
    USUBJID = paste0("test_study", "-", cm_raw$PATNUM)
  ) %>%
  # derive_seq(tgt_var = "VSSEQ",
  #            rec_vars= c("USUBJID", "CMTRT")) %>%
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
  dplyr::select("STUDYID", "USUBJID", everything())
