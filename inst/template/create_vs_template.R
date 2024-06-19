# Name: VS domain
#
# Label: R program to create VS Domain
#
# Input raw data: vitals_raw
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

vs_raw <- read.csv(system.file("raw_data/vitals_raw_data.csv",
  package = "sdtm.oak"
)) %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "vitals"
  )

dm <- read.csv(system.file("raw_data/dm.csv",
  package = "sdtm.oak"
))

# Create VS domain.
# Create the topic variable and corresponding qualifiers for the VS domain.

# Map topic variable SYSBP and its qualifiers.
vs_sysbp <-
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "SYS_BP",
    tgt_var = "VSTESTCD",
    tgt_val = "SYSBP",
    ct_spec = study_ct,
    ct_clst = "C66741"
  ) %>%
  # Filter for records where VSTESTCD is not empty.
  # Only these records need qualifier mappings.
  dplyr::filter(!is.na(.data$VSTESTCD)) %>%
  # Map VSTEST using hardcode_ct algorithm
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "SYS_BP",
    tgt_var = "VSTEST",
    tgt_val = "Systolic Blood Pressure",
    ct_spec = study_ct,
    ct_clst = "C67153",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSORRES using assign_no_ct algorithm
  assign_no_ct(
    raw_dat = vs_raw,
    raw_var = "SYS_BP",
    tgt_var = "VSORRES",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSORRESU using hardcode_ct algorithm
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "SYS_BP",
    tgt_var = "VSORRESU",
    tgt_val = "mmHg",
    ct_spec = study_ct,
    ct_clst = "C66770",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSPOS using assign_ct algorithm
  assign_ct(
    raw_dat = vs_raw,
    raw_var = "SUBPOS",
    tgt_var = "VSPOS",
    ct_spec = study_ct,
    ct_clst = "C71148",
    id_vars = oak_id_vars()
  )

# Map topic variable DIABP and its qualifiers.
vs_diabp <-
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "DIA_BP",
    tgt_var = "VSTESTCD",
    tgt_val = "DIABP",
    ct_spec = study_ct,
    ct_clst = "C66741"
  ) %>%
  dplyr::filter(!is.na(.data$VSTESTCD)) %>%
  # Map VSTEST using hardcode_ct algorithm
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "DIA_BP",
    tgt_var = "VSTEST",
    tgt_val = "Diastolic Blood Pressure",
    ct_spec = study_ct,
    ct_clst = "C67153",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSORRES using assign_no_ct algorithm
  assign_no_ct(
    raw_dat = vs_raw,
    raw_var = "DIA_BP",
    tgt_var = "VSORRES",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSORRESU using hardcode_ct algorithm
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "DIA_BP",
    tgt_var = "VSORRESU",
    tgt_val = "mmHg",
    ct_spec = study_ct,
    ct_clst = "C66770",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSPOS using assign_ct algorithm
  assign_ct(
    raw_dat = vs_raw,
    raw_var = "SUBPOS",
    tgt_var = "VSPOS",
    ct_spec = study_ct,
    ct_clst = "C71148",
    id_vars = oak_id_vars()
  )

# Map topic variable PULSE and its qualifiers.
vs_pulse <-
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "PULSE",
    tgt_var = "VSTESTCD",
    tgt_val = "PULSE",
    ct_spec = study_ct,
    ct_clst = "C66741"
  ) %>%
  dplyr::filter(!is.na(.data$VSTESTCD)) %>%
  # Map VSTEST using hardcode_ct algorithm
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "PULSE",
    tgt_var = "VSTEST",
    tgt_val = "Pulse Rate",
    ct_spec = study_ct,
    ct_clst = "C67153",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSORRES using assign_no_ct algorithm
  assign_no_ct(
    raw_dat = vs_raw,
    raw_var = "PULSE",
    tgt_var = "VSORRES",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSORRESU using hardcode_ct algorithm
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "PULSE",
    tgt_var = "VSORRESU",
    tgt_val = "beats/min",
    ct_spec = study_ct,
    ct_clst = "C66770",
    id_vars = oak_id_vars()
  )

# Map topic variable RESP from the raw variable RESPRT and its qualifiers.
vs_resp <-
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "RESPRT",
    tgt_var = "VSTESTCD",
    tgt_val = "RESP",
    ct_spec = study_ct,
    ct_clst = "C66741"
  ) %>%
  dplyr::filter(!is.na(.data$VSTESTCD)) %>%
  # Map VSTEST using hardcode_ct algorithm
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "RESPRT",
    tgt_var = "VSTEST",
    tgt_val = "Respiratory Rate",
    ct_spec = study_ct,
    ct_clst = "C67153",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSORRES using assign_no_ct algorithm
  assign_no_ct(
    raw_dat = vs_raw,
    raw_var = "RESPRT",
    tgt_var = "VSORRES",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSORRESU using hardcode_ct algorithm
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "RESPRT",
    tgt_var = "VSORRESU",
    tgt_val = "breaths/min",
    ct_spec = study_ct,
    ct_clst = "C66770",
    id_vars = oak_id_vars()
  )

# Map topic variable TEMP from raw variable TEMP and its qualifiers.
vs_temp <-
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "TEMP",
    tgt_var = "VSTESTCD",
    tgt_val = "TEMP",
    ct_spec = study_ct,
    ct_clst = "C66741"
  ) %>%
  dplyr::filter(!is.na(.data$VSTESTCD)) %>%
  # Map VSTEST using hardcode_ct algorithm
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "TEMP",
    tgt_var = "VSTEST",
    tgt_val = "Temperature",
    ct_spec = study_ct,
    ct_clst = "C67153",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSORRES using assign_no_ct algorithm
  assign_no_ct(
    raw_dat = vs_raw,
    raw_var = "TEMP",
    tgt_var = "VSORRES",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSORRESU using hardcode_ct algorithm
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "TEMP",
    tgt_var = "VSORRESU",
    tgt_val = "C",
    ct_spec = study_ct,
    ct_clst = "C66770",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSLOC from TEMPLOC using assign_ct
  assign_ct(
    raw_dat = vs_raw,
    raw_var = "TEMPLOC",
    tgt_var = "VSLOC",
    ct_spec = study_ct,
    ct_clst = "C74456",
    id_vars = oak_id_vars()
  )

# Map topic variable OXYSAT from raw variable OXY_SAT and its qualifiers.
vs_oxysat <-
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "OXY_SAT",
    tgt_var = "VSTESTCD",
    tgt_val = "OXYSAT",
    ct_spec = study_ct,
    ct_clst = "C66741"
  ) %>%
  dplyr::filter(!is.na(.data$VSTESTCD)) %>%
  # Map VSTEST using hardcode_ct algorithm
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "OXY_SAT",
    tgt_var = "VSTEST",
    tgt_val = "Oxygen Saturation",
    ct_spec = study_ct,
    ct_clst = "C67153",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSORRES using assign_no_ct algorithm
  assign_no_ct(
    raw_dat = vs_raw,
    raw_var = "OXY_SAT",
    tgt_var = "VSORRES",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSORRESU using hardcode_ct algorithm
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "OXY_SAT",
    tgt_var = "VSORRESU",
    tgt_val = "%",
    ct_spec = study_ct,
    ct_clst = "C66770",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSLAT using assign_ct from raw variable LAT
  assign_ct(
    raw_dat = vs_raw,
    raw_var = "LAT",
    tgt_var = "VSLAT",
    ct_spec = study_ct,
    ct_clst = "C99073",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSLOC using assign_ct from raw variable LOC
  assign_ct(
    raw_dat = vs_raw,
    raw_var = "LOC",
    tgt_var = "VSLOC",
    ct_spec = study_ct,
    ct_clst = "C74456",
    id_vars = oak_id_vars()
  )

# Map topic variable VSALL from raw variable ASMNTDN with the logic if ASMNTDN  == 1 then VSTESTCD = VSALL
vs_vsall <-
  hardcode_ct(
    raw_dat = condition_add(vs_raw, ASMNTDN == 1L),
    raw_var = "ASMNTDN",
    tgt_var = "VSTESTCD",
    tgt_val = "VSALL",
    ct_spec = study_ct,
    ct_clst = "C66741"
  ) %>%
  dplyr::filter(!is.na(.data$VSTESTCD)) %>%
  # Map VSTEST using hardcode_ct algorithm
  hardcode_ct(
    raw_dat = vs_raw,
    raw_var = "ASMNTDN",
    tgt_var = "VSTEST",
    tgt_val = "Vital Signs",
    ct_spec = study_ct,
    ct_clst = "C67153",
    id_vars = oak_id_vars()
  )

# Combine all the topic variables into a single data frame.
vs_combined <- dplyr::bind_rows(
  vs_vsall, vs_sysbp, vs_diabp, vs_pulse, vs_resp,
  vs_temp, vs_oxysat
)

# Map qualifiers common to all topic variables

vs <- vs_combined %>%
  # Map VSDTC using assign_ct algorithm
  assign_datetime(
    raw_dat = vs_raw,
    raw_var = c("VTLD", "VTLTM"),
    tgt_var = "VSDTC",
    raw_fmt = c(list(c("d-m-y", "dd-mmm-yyyy")), "H:M")
  ) %>%
  # Map VSTPT from TMPTC using assign_ct
  assign_ct(
    raw_dat = vs_raw,
    raw_var = "TMPTC",
    tgt_var = "VSTPT",
    ct_spec = study_ct,
    ct_clst = "TPT",
    id_vars = oak_id_vars()
  ) %>%
  # Map VSTPTNUM from TMPTC using assign_ct
  assign_ct(
    raw_dat = vs_raw,
    raw_var = "TMPTC",
    tgt_var = "VSTPTNUM",
    ct_spec = study_ct,
    ct_clst = "TPTNUM",
    id_vars = oak_id_vars()
  ) %>%
  # Map VISIT from INSTANCE using assign_ct
  assign_ct(
    raw_dat = vs_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  # Map VISITNUM from INSTANCE using assign_ct
  assign_ct(
    raw_dat = vs_raw,
    raw_var = "INSTANCE",
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
  # derive_seq(tgt_var = "VSSEQ",
  #            rec_vars= c("USUBJID", "CMTRT")) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "VSDTC",
    refdt = "RFXSTDTC",
    study_day_var = "VSDY"
  ) %>%
  dplyr::select("STUDYID", "DOMAIN", "USUBJID", everything())
