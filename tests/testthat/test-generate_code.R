test_that("generate_code works", {

  # nolint start
  spec <- tibble::tribble(
    ~study_number,  ~raw_dataset,        ~raw_dataset_label, ~raw_variable, ~raw_variable_label, ~raw_variable_ordinal, ~raw_variable_type, ~raw_data_format, ~study_specific, ~annotation_ordinal, ~mapping_is_dataset, ~annotation_text, ~target_sdtm_domain, ~target_sdtm_variable, ~target_sdtm_variable_role, ~target_sdtm_variable_codelist_code, ~target_sdtm_variable_controlled_terms_or_format, ~target_sdtm_variable_ordinal, ~origin, ~mapping_algorithm, ~entity_sub_algorithm, ~target_hardcoded_value, ~target_term_value, ~target_term_code, ~condition_ordinal, ~condition_group_ordinal, ~condition_add_raw_dat, ~condition_add_tgt_dat, ~condition_left_raw_dataset, ~condition_left_raw_variable, ~condition_left_sdtm_domain, ~condition_left_sdtm_variable, ~condition_operator, ~condition_right_text_value, ~condition_right_sdtm_domain, ~condition_right_sdtm_variable, ~condition_right_raw_dataset, ~condition_right_raw_variable, ~condition_next_logical_operator, ~merge_type, ~merge_left, ~merge_right, ~merge_condition, ~unduplicate_keys, ~groupby_keys, ~target_resource_raw_dataset, ~target_resource_raw_variable,
       "lp_study", "cm_raw_data", "Concomitant Medications",    "IT.CMTRT",         "var label",                   "3",             "Text",           "$200",         "FALSE",                 "1",             "FALSE",       "CM.CMTRT",                "CM",               "CMTRT",           "Topic Variable",                                  NA,                                               NA,                          "10",   "CRF",     "assign_no_ct",                    NA,                      NA,                 NA,                NA,                 NA,                       NA,                     NA,                     NA,                          NA,                           NA,                          NA,                            NA,                  NA,                          NA,                           NA,                             NA,                           NA,                            NA,                               NA,          NA,          NA,           NA,               NA,                NA,            NA,                           NA,                            NA,
       "lp_study", "cm_raw_data", "Concomitant Medications",   "IT.CMINDC",         "var label",                   "4",             "Text",           "$100",         "FALSE",                 "1",             "FALSE",      "CM.CMINDC",                "CM",              "CMINDC",         "Record Qualifier",                                  NA,                                               NA,                          "19",   "CRF",     "assign_no_ct",                    NA,                      NA,                 NA,                NA,                 NA,                       NA,                     NA,                     NA,                          NA,                           NA,                          NA,                            NA,                  NA,                          NA,                           NA,                             NA,                           NA,                            NA,                               NA,          NA,          NA,           NA,               NA,                NA,            NA,                           NA,                            NA
    )
  # nolint end

  domain <- "cm"

  temp_dir <- tempdir()
  out_dir <- file.path(temp_dir, "data/generate_code")
  unlink(out_dir, recursive = TRUE, force = TRUE)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  withr::with_options(list(width = 20), {
    generate_code(spec, domain, out_dir)
    observed <- readLines(file.path(out_dir,  paste0(domain, "_sdtm_oak_code.R")))

    expect_true(identical(length(observed), 10L))
    expect_true(grepl("CMTRT", observed[3]))
  })
})
