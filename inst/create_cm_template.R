# Name: CM domain
#
# Label: R program to create CM Domain
#
# Input raw data:
# study_sdtm_spec
# study_controlled_terminology
# study_raw_datasets
#
#

library(sdtm.oak)
library(dplyr)

# Read Specification

# sdtm_spec <- read_sdtm_spec(filename = "~/study/study_sdtm_spec.csv")

study_ct <- read_study_ct(filename = "~/study/study_sdtm_ct.csv")

# Read in raw data

MD1 <- read_raw_data_csv(filename = "~/study/MD1.csv") |>
  # Derive oak_id_vars
  derive_oak_id_vars()

# Create CM domain. The first step in creating CM domain is to create the topic variable

cm <- MD1 |>
  # Derive topic variable
  assign_no_ct(
    raw_dataset = MD1, # This is added for pseudocode. Not required as pipe will send it
    raw_variable = MDRAW,
    target_sdtm_var = CMTRT
  ) |>
  # Derive qualifier CMDOSU
  # Use merge and add the qualifier to the topic variable
  assign_ct(
    raw_dataset = MD1,
    raw_variable = DOSU,
    target_sdtm_var = CMDOSU,
    study_ct = study_ct,
    target_sdtm_variable_codelist_code = "C71620",
    merge_to_topic_by = c(oak_id_vars,
      topic_var_source = MDRAW
    )
  ) |>
  # Derive qualifier CMDOSFRM and merge it with the target dataset
  assign_ct(
    raw_dataset = MD1,
    raw_variable = MDFORM,
    target_sdtm_var = CMDOSFRM,
    target_sdtm_variable_codelist_code = "C66726",
    study_ct = study_ct,
    merge_to_topic_by = c(oak_id_vars,
      topic_var_source = MDRAW
    )
  ) |>
  # DERIVE CMROUTE
  assign_ct(
    raw_dataset = MD1,
    raw_variable = MDFORM,
    target_sdtm_var = MDRTE,
    target_sdtm_variable_codelist_code = "C66729",
    merge_to_topic_by = c(oak_id_vars,
      topic_var_source = MDRAW
    )
  ) |>
  # DERIVE CMDOSFRQ
  assign_ct(
    raw_dataset = MD1,
    raw_variable = MDFRQ,
    target_sdtm_var = CMDOSFRQ,
    target_sdtm_variable_codelist_code = "C71113",
    merge_to_topic_by = c(oak_id_vars,
      topic_var_source = MDRAW
    )
  ) |>
  # DERIVE CMINDC
  assign_no_ct(
    raw_dataset = MD1,
    raw_variable = MDIND,
    target_sdtm_var = CMINDC,
    merge_to_topic_by = c(oak_id_vars,
      topic_var_source = MDRAW
    )
  ) |>
  # Derive qualifier CMDOS. Annotation text = If numeric then CM.CMDOSE
  if_then_else(
    raw_dataset = MD1,
    raw_variable = DOS,
    condition_left_raw_dataset = MD1,
    condition_left_raw_variable = DOS,
    condition_operator = "is_numeric",
    sub_algorithm = assign_no_ct, # pass the function as the argument
    target_sdtm_var = CMDOSE,
    merge_to_topic_by = c(oak_id_vars,
      topic_var_source = MDRAW
    )
  ) |>
  # Derive qualifier CMDOSTXT.  Annotation text = If character then CM.CMDOSTXT
  if_then_else(
    raw_dataset = MD1,
    raw_variable = DOS,
    condition_left_raw_dataset = MD1,
    condition_left_raw_variable = DOS,
    condition_operator = "is_character",
    sub_algorithm = assign_no_ct,
    target_sdtm_var = CMDOSETXT,
    merge_to_topic_by = c(oak_id_vars,
      topic_var_source = MDRAW
    )
  ) |>
  # Derive qualifier CMMODIFY  Annotation text = If different to CM.CMTRT then CM.CMMODIFY
  if_then_else(
    raw_dataset = MD1,
    raw_variable = CMMODIFY,
    condition_left_raw_dataset = MD1,
    condition_left_raw_variable = CMMODIFY,
    condition_operator = "diffferent_to",
    condition_right_sdtm_variable_domain = CM,
    condition_right_sdtm_variable = CMTRT,
    sub_algorithm = assign_no_ct,
    target_sdtm_var = CMDOSETXT,
    merge_to_topic_by = c(oak_id_vars,
      topic_var_source = MDRAW
    )
  ) |>
  # Derive CMDECOD
  assign_no_ct(
    raw_dataset = MD1,
    raw_variable = CMDECOD,
    target_sdtm_var = CMDECOD
  ) |>
  # Derive CMSTDTC. This function calls create_iso8601
  assign_no_ct(
    raw_dataset = MD1,
    raw_variable = c("MDBD", "MDBTM"),
    target_sdtm_var = CMSTDTC,
    .format = c("ddmmmyyyy", "HHMM")
  ) |>
  # Derive CMENDTC. This function calls create_iso8601
  assign_no_ct(
    raw_dataset = MD1,
    raw_variable = c("MDED", "MDETM"),
    target_sdtm_var = CMENDTC,
    .format = c("ddmmmyyyy", "HHMM")
  ) |>
  # Derive qualifier CMSTRTPT  Annotation text = If checked then CM.CMSTRTPT = 'BEFORE'
  if_then_else(
    raw_dataset = MD1,
    raw_variable = MDPRIOR,
    condition_left_raw_dataset = MD1,
    condition_left_raw_variable = MDPRIOR,
    condition_operator = "if_checked",
    sub_algorithm = assign_ct,
    target_sdtm_variable_codelist_code = "C66728",
    target_sdtm_var = CMSTRTPT,
    merge_to_topic_by = c(oak_id_vars,
      topic_var_source = MDRAW
    )
  ) |>
  # Derive qualifier CMSTRTPT  Annotation text = If checked then CM.CMSTTPT = 'SCREENING'
  if_then_else(
    raw_dataset = MD1,
    raw_variable = MDPRIOR,
    condition_left_raw_dataset = MD1,
    condition_left_raw_variable = MDPRIOR,
    condition_operator = "if_checked",
    sub_algorithm = hardcode_no_ct,
    target_hardcoded_value = "SCREENING",
    target_sdtm_var = CM.CMSTTPT,
    merge_to_topic_by = c(oak_id_vars,
      topic_var_source = MDRAW
    )
  ) |>
  # Derive qualifier CMSTRTPT  Annotation text = If checked then CM.CMENRTPT = 'ONGOING'
  if_then_else(
    raw_dataset = MD1,
    raw_variable = MDONG,
    condition_left_raw_dataset = MD1,
    condition_left_raw_variable = MDONG,
    condition_operator = "if_checked",
    sub_algorithm = hardcode_ct,
    target_hardcoded_value = "ONGOING",
    target_sdtm_variable_codelist_code = "C66728",
    target_sdtm_var = CMENRTPT,
    merge_to_topic_by = c(oak_id_vars,
      topic_var_source = MDRAW
    )
  ) |>
  # Derive qualifier CMSTRTPT  Annotation text = If checked then CM.CMENTPT = 'DATE OF LAST ASSESSMENT'
  if_then_else(
    raw_dataset = MD1,
    raw_variable = MDONG,
    condition_left_raw_dataset = MD1,
    condition_left_raw_variable = MDONG,
    condition_operator = "if_checked",
    sub_algorithm = hardcode_no_ct,
    target_hardcoded_value = "DATE OF LAST ASSESSMENT",
    target_sdtm_var = CMENTPT,
    merge_to_topic_by = c(oak_id_vars,
      topic_var_source = MDRAW
    )
  ) |>
  # Derive CMGRPID
  assign_no_ct(
    raw_dataset = MD1,
    raw_variable = MDNUM,
    target_sdtm_var = CMGRPID,
    merge_to_topic_by = c(oak_id_vars,
      topic_var_source = MDRAW
    )
  ) |>
  dplyr::mutate(
    STUDYID = "test_study",
    DOMAIN = "CM",
    CMCAT = "GENERAL CONMED"
  ) |>
  derive_usubjid() |>
  derive_sequence(keys = c(USUBJID, CMTRT)) |>
  derive_visit_visitnum() |>
  derive_study_day(
    var_in = CMSTDTC,
    target_var = CMSTDY
  ) |>
  derive_study_day(
    var_in = CMENDTC,
    target_var = CMENDY
  )
