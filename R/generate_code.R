#' Generate the code for the mapping SDTM specification
#'
#' @param spec The specification data frame.
#' @param domain The SDTM domain to generate the code for.
#' @param out_dir The directory to save the code file. Default is the current
#'   directory.
#'
#' @return Side effect: the code is generated and saved to a file.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' spec <- read_spec("cm_sdtm_oak_spec_cdash.csv")
#' domain <- "cm"
#' generate_code(spec, domain)
#' }
#'
generate_code <- function(spec, domain, out_dir = ".") {

  admiraldev::assert_data_frame(spec, required_vars = rlang::parse_exprs(expected_columns))
  admiraldev::assert_character_scalar(domain)

  # For now assuming that there is only one topic and the topic is the first one

  spec_domain <- spec |>
    dplyr::filter(tolower(target_sdtm_domain) %in% tolower(domain)) |>
    # TODO
    # Doing only few variables
    dplyr::filter(target_sdtm_variable %in% c(
      "CMTRT",
      "CMINDC",
      "CMDOSE"
    )) |>

    dplyr::select(
      raw_dataset,
      raw_variable,
      target_sdtm_variable,
      mapping_algorithm,
      entity_sub_algorithm,
      condition_add_raw_dat,
    )

  # For now swapping entity_sub_algorithm with mapping_algorithm since the
  # algorithms like assign_no_ct are the mapping_algorithm and they are populated
  # in the entity_sub_algorithm
  spec_domain <- spec_domain |>
    dplyr::mutate(
      entity_sub_algorithm_temp = dplyr::if_else(
        mapping_algorithm %in% "condition_add",
        mapping_algorithm,
        entity_sub_algorithm,
      ),
      mapping_algorithm = dplyr::if_else(
        mapping_algorithm %in% "condition_add",
        entity_sub_algorithm,
        mapping_algorithm,
      ),
      entity_sub_algorithm = entity_sub_algorithm_temp
    ) |>
    dplyr::select(-entity_sub_algorithm_temp)


  n_rows <- nrow(spec_domain)

  # Generate the code for each variable row in spec_domain
  code_blocks <- purrr::map(
    seq_len(n_rows),
    \(row) generate_one_var_code(
      spec_domain[row, ],
      last_var = identical(row, n_rows)
    )
  ) |>
    unlist()

  styled_code <- styler::style_text(code_blocks)

  # Save the code to a file
  file_name <- paste0(domain, "_sdtm_oak_code.R")
  writeLines(styled_code, file.path(out_dir, file_name))
}

#' Generate the code for one variable
#'
#' @param spec_var The specification for one variable.
#' @param last_var Logical indicating if this is the last variable in the domain.
#'
#' @return The code for the variable as a string.
#' @keywords internal
#'
generate_one_var_code <- function(spec_var, last_var = FALSE) {

  admiraldev::assert_data_frame(spec_var)

  # Generate the function call
  generated_call <- with(spec_var, {
    rlang::call2(
      mapping_algorithm,
      raw_dat = rlang::sym(raw_dataset),
      raw_var = raw_variable,
      tgt_var = target_sdtm_variable
    )
  })

  # Convert the call to code as a string. Intentionally limiting the width to 20
  # characters to force each parameter to be on a separate line.
  raw_code <- rlang::expr_deparse(generated_call, width = 20)

  # Add the pipe operator if this is not the last variable
  if (!last_var) {
    raw_code[length(raw_code)] <- paste0(raw_code[length(raw_code)], " %>%")
  }

  return(raw_code)
}


#' Read the specification file
#'
#' @param file The path to the specification file.
#'
#' @returns A tibble with the specification.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' file <- "cm_sdtm_oak_spec_cdash.csv"
#' observed <- read_spec(file)
#' }
#'
read_spec <- function(file) {

  admiraldev::assert_character_scalar(file)

  spec <- utils::read.csv(file = file, na.strings = c("NA", ""), colClasses = "character") |>
    tibble::as_tibble()

  admiraldev::assert_data_frame(spec, required_vars = rlang::parse_exprs(expected_columns))

  return(spec)
}

#' Expected columns in the specification file
#'
#' @keywords internal
#'
expected_columns <- c(
  "study_number",
  "raw_dataset",
  "raw_dataset_label",
  "raw_variable",
  "raw_variable_label",
  "raw_variable_ordinal",
  "raw_variable_type",
  "raw_data_format",
  "study_specific",
  "annotation_ordinal",
  "mapping_is_dataset",
  "annotation_text",
  "target_sdtm_domain",
  "target_sdtm_variable",
  "target_sdtm_variable_role",
  "target_sdtm_variable_codelist_code",
  "target_sdtm_variable_controlled_terms_or_format",
  "target_sdtm_variable_ordinal",
  "origin",
  "mapping_algorithm",
  "entity_sub_algorithm",
  "target_hardcoded_value",
  "target_term_value",
  "target_term_code",
  "condition_ordinal",
  "condition_group_ordinal",
  "condition_add_raw_dat",
  "condition_add_tgt_dat",
  "condition_left_raw_dataset",
  "condition_left_raw_variable",
  "condition_left_sdtm_domain",
  "condition_left_sdtm_variable",
  "condition_operator",
  "condition_right_text_value",
  "condition_right_sdtm_domain",
  "condition_right_sdtm_variable",
  "condition_right_raw_dataset",
  "condition_right_raw_variable",
  "condition_next_logical_operator",
  "merge_type",
  "merge_left",
  "merge_right",
  "merge_condition",
  "unduplicate_keys",
  "groupby_keys",
  "target_resource_raw_dataset",
  "target_resource_raw_variable"
)
