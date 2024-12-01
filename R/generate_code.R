#' Generate the code for the mapping SDTM specification
#'
#' One can use the option `width` to control the width of the code. A width of
#' twenty will almost always place every parameter on a separate line. This is
#' useful for debugging and understanding the code. The higher the width, the
#' more parameters will be placed on a single line and code will be shorter.
#' See the examples for more details.
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
#'
#' # One can use option width to control the width of the code
#' # Twenty will almost always place every parameter on a separate line
#' options(width = 20)
#' generate_code(spec, domain)
#' }
#'
generate_code <- function(spec, domain, out_dir = ".") {

  admiraldev::assert_data_frame(spec, required_vars = rlang::syms(expected_columns))
  admiraldev::assert_character_scalar(domain)

  spec_domain <- get_domain_spec(spec, domain)

  # Generate the code for each variable row in spec_domain
  styled_code <- purrr::map(
    seq_len(nrow(spec_domain)),
    \(row) generate_one_var_code(spec_domain[row, ])
  ) |>
    unlist() |>
    remove_last_pipe() |>
    styler::style_text()

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
generate_one_var_code <- function(spec_var) {

  admiraldev::assert_data_frame(spec_var)
  assertthat::assert_that(identical(nrow(spec_var), 1L))

  args <- list(
    raw_dat = rlang::parse_expr(spec_var$raw_dataset),
    raw_var = spec_var$raw_variable,
    tgt_var = spec_var$target_sdtm_variable
  )

  is_ct <- spec_var$mapping_algorithm %in% c("assign_ct")

  if (is_ct) {
    args$ct_spec <- rlang::parse_expr("study_ct")
    args$ct_clst <- spec_var$target_sdtm_variable_codelist_code
  }

  # Generate the function call
  generated_call <- rlang::call2(
    spec_var$mapping_algorithm,
    !!!args
  )

  rlang::expr_deparse(generated_call) |>
    add_pipe()
}


#' Add a pipe operator to the last element of a character vector
#'
#' @param code_block A character vector.
#'
#' @return The character vector with a pipe operator added to the last element.
#' @keywords internal
#'
add_pipe <- function(code_block) {

  admiraldev::assert_character_vector(code_block)

  i <- length(code_block)

  # Add pipe operator to the last element of code block
  code_block[i] <- paste0(code_block[i], " %>%")
  code_block
}

#' Remove the pipe operator from the last element of a character vector
#'
#' @param code_blocks A character vector.
#'
#' @return The character vector with the pipe operator removed from the last element.
#' @keywords internal
#'
remove_last_pipe <- function(code_blocks) {

  admiraldev::assert_character_vector(code_blocks)

  len_code_block <- length(code_blocks)

  # The last code block should not have a pipe operator
  code_blocks[len_code_block] <- code_blocks[len_code_block] |>
    stringr::str_remove("%>%")

  code_blocks
}

#' Get the specification for a domain and modify it
#'
#' @param spec The specification data frame.
#' @param domain The SDTM domain to get the specification for.
#'
#' @return
#' @keywords internal
#'
#' @examples
get_domain_spec <- function(spec, domain) {

  expected_columns <- c(
    "raw_dataset",
    "raw_variable",
    "target_sdtm_variable",
    "mapping_algorithm",
    "entity_sub_algorithm",
    "condition_add_raw_dat",
    "target_sdtm_variable_codelist_code"
  )

  admiraldev::assert_data_frame(spec, required_vars = rlang::syms(expected_columns))
  admiraldev::assert_character_scalar(domain)

  # TODO
  # Temporary vector to control the vars we generate and the order
  tgt_vars <- c(
    "CMTRT",
    "CMINDC",
    "CMDOSE",
    "CMDOSTXT",
    "CMDOSU"
  )

  # For now assuming that there is only one topic and the topic is the first one

  spec |>
    dplyr::filter(tolower(target_sdtm_domain) %in% tolower(domain)) |>
    # TODO
    # Doing only few variables
    dplyr::filter(target_sdtm_variable %in% tgt_vars) |>

    dplyr::select(dplyr::all_of(expected_columns)) |>

    # For now swapping entity_sub_algorithm with mapping_algorithm since the
    # algorithms like assign_no_ct are the mapping_algorithm and they are populated
    # in the entity_sub_algorithm
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
    dplyr::select(-entity_sub_algorithm_temp) |>

    # Need to use the condition_add_raw_dat (if not missing) instead of raw_dataset
    dplyr::mutate(
      raw_dataset = dplyr::if_else(
        entity_sub_algorithm %in% "condition_add" & !is.na(condition_add_raw_dat),
        condition_add_raw_dat,
        raw_dataset
      )
    )
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

  admiraldev::assert_data_frame(spec, required_vars = rlang::syms(expected_columns))

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
