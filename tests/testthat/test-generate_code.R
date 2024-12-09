test_that("generate_code works for one topic domain", {
  # nolint start
  spec <- tibble::tribble(
    ~study_number,  ~raw_dataset,        ~raw_dataset_label, ~raw_variable, ~raw_variable_label, ~raw_variable_ordinal, ~raw_variable_type, ~raw_data_format, ~raw_fmt, ~raw_unk, ~study_specific, ~annotation_ordinal, ~mapping_is_dataset, ~annotation_text, ~target_sdtm_domain, ~target_sdtm_variable, ~target_sdtm_variable_role,   ~topic, ~target_sdtm_variable_codelist_code, ~target_sdtm_variable_controlled_terms_or_format, ~target_sdtm_variable_ordinal, ~origin, ~mapping_algorithm, ~entity_sub_algorithm, ~target_hardcoded_value, ~target_term_value, ~target_value, ~target_term_code, ~condition_ordinal, ~condition_group_ordinal, ~condition_add_raw_dat, ~condition_add_tgt_dat, ~condition_left_raw_dataset, ~condition_left_raw_variable, ~condition_left_sdtm_domain, ~condition_left_sdtm_variable, ~condition_operator, ~condition_right_text_value, ~condition_right_sdtm_domain, ~condition_right_sdtm_variable, ~condition_right_raw_dataset, ~condition_right_raw_variable, ~condition_next_logical_operator, ~merge_type, ~merge_left, ~merge_right, ~merge_condition, ~unduplicate_keys, ~groupby_keys, ~target_resource_raw_dataset, ~target_resource_raw_variable,
    "lp_study",    "cm_raw_data", "Concomitant Medications",    "IT.CMTRT",         "var label",                   "3",             "Text",           "$200",       NA,       NA,         "FALSE",                 "1",             "FALSE",       "CM.CMTRT",                "CM",               "CMTRT",           "Topic Variable",  "CMTRT",                                NA,                                               NA,                          "10",   "CRF",     "assign_no_ct",                    NA,                      NA,                 NA,            NA,                NA,                 NA,                       NA,                     NA,                     NA,                          NA,                           NA,                          NA,                            NA,                  NA,                          NA,                           NA,                             NA,                           NA,                            NA,                               NA,          NA,          NA,           NA,               NA,                NA,            NA,                           NA,                            NA,
    "lp_study",    "cm_raw_data", "Concomitant Medications",   "IT.CMINDC",         "var label",                   "4",             "Text",           "$100",       NA,       NA,         "FALSE",                 "1",             "FALSE",      "CM.CMINDC",                "CM",              "CMINDC",         "Record Qualifier",  "CMTRT",                                NA,                                               NA,                          "19",   "CRF",     "assign_no_ct",                    NA,                      NA,                 NA,            NA,                NA,                 NA,                       NA,                     NA,                     NA,                          NA,                           NA,                          NA,                            NA,                  NA,                          NA,                           NA,                             NA,                           NA,                            NA,                               NA,          NA,          NA,           NA,               NA,                NA,            NA,                           NA,                            NA
  )
  # nolint end

  # Convert all NA to NA_character_
  spec <- spec |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::everything(),
      .fns = \(x) dplyr::if_else(is.na(x), NA_character_, x)
    ))

  domain <- "cm"

  temp_dir <- tempdir()
  out_dir <- file.path(temp_dir, "data/generate_code")
  unlink(out_dir, recursive = TRUE, force = TRUE)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  withr::with_options(list(width = 20L), {
    generate_code(spec, domain, out_dir)
    observed <- readLines(file.path(out_dir, paste0(domain, "_sdtm_oak_code.R")))

    expect_gt(length(observed), 10L)
    # From prefix
    expect_true(grepl("generate_oak_id_vars", observed, fixed = TRUE) |> any())
    # From generator
    expect_true(grepl("assign_no_ct", observed, fixed = TRUE) |> any())
    # From suffix
    expect_true(grepl("dplyr::select", observed, fixed = TRUE) |> any())
  })
})

test_that("generate_code works for multiple topics domain", {

  # nolint start
  spec <- tibble::tribble(
    ~study_number,      ~raw_dataset, ~raw_dataset_label, ~raw_variable,       ~raw_variable_label, ~raw_variable_ordinal, ~raw_variable_type, ~raw_data_format, ~raw_fmt, ~raw_unk, ~study_specific, ~annotation_ordinal, ~mapping_is_dataset,                                                   ~annotation_text, ~target_sdtm_domain, ~target_sdtm_variable, ~target_sdtm_variable_role,    ~topic, ~target_sdtm_variable_codelist_code, ~target_sdtm_variable_controlled_terms_or_format, ~target_sdtm_variable_ordinal,    ~origin, ~mapping_algorithm, ~entity_sub_algorithm, ~target_hardcoded_value,        ~target_term_value,             ~target_value, ~target_term_code, ~condition_ordinal, ~condition_group_ordinal,                               ~condition_add_raw_dat, ~condition_add_tgt_dat, ~condition_left_raw_dataset, ~condition_left_raw_variable, ~condition_left_sdtm_domain, ~condition_left_sdtm_variable, ~condition_operator, ~condition_right_text_value, ~condition_right_sdtm_domain, ~condition_right_sdtm_variable, ~condition_right_raw_dataset, ~condition_right_raw_variable, ~condition_next_logical_operator, ~merge_type, ~merge_left, ~merge_right, ~merge_condition, ~unduplicate_keys, ~groupby_keys, ~target_resource_raw_dataset, ~target_resource_raw_variable,
    "lp_study",    "vitals_raw_data",      "Vital Signs",     "ASMNTDN",     "Assessment not done",                   "1",     "DropDownList",              "1",       NA,       NA,         "FALSE",                 "1",             "FALSE",     "If No then VS.VSSTAT = 'NOT DONE' when VS.VSTESTCD = 'VSALL'",                "VS",            "VSTESTCD",           "Topic Variable", "ASMNTDN",                            "C66741",                                     "(VSTESTCD)",                           "7", "Assigned",    "condition_add",         "hardcode_ct",                      NA,                   "VSALL",                   "VSALL",          "V00224",              "1.3",                      "0", "condition_add(vitals_raw_data, ASMNTDN == \"Yes\")",                     NA,           "vitals_raw_data",                    "ASMNTDN",                          NA,                            NA,          "equal_to",                       "Yes",                           NA,                             NA,                           NA,                            NA,                               NA,          NA,          NA,           NA,               NA,                NA,            NA,                           NA,                            NA,
    "lp_study",    "vitals_raw_data",      "Vital Signs",     "ASMNTDN",     "Assessment not done",                   "1",     "DropDownList",              "1",       NA,       NA,         "FALSE",                 "2",             "FALSE",                             "If No then VS.VSTEST = 'Vital Signs'",                "VS",              "VSTEST",        "Synonym Qualifier", "ASMNTDN",                            "C67153",                                       "(VSTEST)",                           "8", "Assigned",    "condition_add",         "hardcode_ct",                      NA,             "Vital Signs",             "Vital Signs",          "V00224",              "2.1",                      "0", "condition_add(vitals_raw_data, ASMNTDN == \"Yes\")",                     NA,           "vitals_raw_data",                    "ASMNTDN",                          NA,                            NA,          "equal_to",                       "Yes",                           NA,                             NA,                           NA,                            NA,                               NA,          NA,          NA,           NA,               NA,                NA,            NA,                           NA,                            NA,
    "lp_study",    "vitals_raw_data",      "Vital Signs",     "ASMNTDN",     "Assessment not done",                   "1",     "DropDownList",              "1",       NA,       NA,         "FALSE",                 "1",             "FALSE",     "If No then VS.VSSTAT = 'NOT DONE' when VS.VSTESTCD = 'VSALL'",                "VS",              "VSSTAT",         "Record Qualifier", "ASMNTDN",                            "C66789",                                           "(ND)",                          "17", "Assigned",    "condition_add",         "hardcode_ct",                      NA,                "NOT DONE",                "NOT DONE",          "C49484",              "1.2",                      "0", "condition_add(vitals_raw_data, ASMNTDN == \"Yes\")",                     NA,           "vitals_raw_data",                    "ASMNTDN",                          NA,                            NA,          "equal_to",                       "Yes",                           NA,                             NA,                           NA,                            NA,                               NA,          NA,          NA,           NA,               NA,                NA,            NA,                           NA,                            NA,
    "lp_study",    "vitals_raw_data",      "Vital Signs",      "SYS_BP", "Systolic blood pressure",                   "6",             "Text",              "3",       NA,       NA,         "FALSE",                 "1",             "FALSE",                            "VS.VSORRES when VS.VSTESTCD = 'SYSBP'",                "VS",            "VSTESTCD",           "Topic Variable",  "SYS_BP",                            "C66741",                                     "(VSTESTCD)",                           "7", "Assigned",      "hardcode_ct",                    NA,                      NA,                   "SYSBP",                   "SYSBP",          "C25298",                 NA,                       NA,                                                   NA,                     NA,                          NA,                           NA,                          NA,                            NA,                  NA,                          NA,                           NA,                             NA,                           NA,                            NA,                               NA,          NA,          NA,           NA,               NA,                NA,            NA,                           NA,                            NA,
    "lp_study",    "vitals_raw_data",      "Vital Signs",      "SYS_BP", "Systolic blood pressure",                   "6",             "Text",              "3",       NA,       NA,         "FALSE",                 "2",             "FALSE",                            "VS.VSTEST = 'Systolic Blood Pressure'",                "VS",              "VSTEST",        "Synonym Qualifier",  "SYS_BP",                            "C67153",                                       "(VSTEST)",                           "8", "Assigned",      "hardcode_ct",                    NA,                      NA, "Systolic Blood Pressure", "Systolic Blood Pressure",          "C25298",                 NA,                       NA,                                                   NA,                     NA,                          NA,                           NA,                          NA,                            NA,                  NA,                          NA,                           NA,                             NA,                           NA,                            NA,                               NA,          NA,          NA,           NA,               NA,                NA,            NA,                           NA,                            NA,
    "lp_study",    "vitals_raw_data",      "Vital Signs",      "SYS_BP", "Systolic blood pressure",                   "6",             "Text",              "3",       NA,       NA,         "FALSE",                 "1",             "FALSE",                            "VS.VSORRES when VS.VSTESTCD = 'SYSBP'",                "VS",             "VSORRES",         "Result Qualifier",  "SYS_BP",                                  NA,                                               NA,                          "12",      "CRF",     "assign_no_ct",                    NA,                      NA,                        NA,                        NA,                NA,                 NA,                       NA,                                                   NA,                     NA,                          NA,                           NA,                          NA,                            NA,                  NA,                          NA,                           NA,                             NA,                           NA,                            NA,                               NA,          NA,          NA,           NA,               NA,                NA,            NA,                           NA,                            NA,
    "lp_study",    "vitals_raw_data",      "Vital Signs",      "SYS_BP", "Systolic blood pressure",                   "6",             "Text",              "3",       NA,       NA,         "FALSE",                 "3",             "FALSE",                                       "VS.VSORRESU = <Fixed Unit>",                "VS",            "VSORRESU",       "Variable Qualifier",  "SYS_BP",                            "C66770",                                       "(VSRESU)",                          "13", "Assigned",      "hardcode_ct",                    NA,                      NA,                    "mmHg",                    "mmHg",          "C49670",                 NA,                       NA,                                                   NA,                     NA,                          NA,                           NA,                          NA,                            NA,                  NA,                          NA,                           NA,                             NA,                           NA,                            NA,                               NA,          NA,          NA,           NA,               NA,                NA,            NA,                           NA,                            NA
    )
  # nolint end

  # Convert all NA to NA_character_
  spec <- spec |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::everything(),
      .fns = \(x) dplyr::if_else(is.na(x), NA_character_, x)
    ))

  domain <- "vs"

  temp_dir <- tempdir()
  out_dir <- file.path(temp_dir, "data/generate_code")
  unlink(out_dir, recursive = TRUE, force = TRUE)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  withr::with_options(list(width = 20L), {
    generate_code(spec, domain, out_dir)
    observed <- readLines(file.path(out_dir, paste0(domain, "_sdtm_oak_code.R")))

    expect_gt(length(observed), 100L)
    # From prefix
    expect_true(grepl("generate_oak_id_vars", observed, fixed = TRUE) |> any())
    # From generator
    expect_true(grepl("assign_no_ct", observed, fixed = TRUE) |> any())
    # From suffix
    expect_true(grepl("dplyr::select", observed, fixed = TRUE) |> any())
  })
})
