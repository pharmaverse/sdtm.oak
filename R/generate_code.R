# TODO Things I changed in specs
# - Made sure that target_sdtm_domain is not missing
# - Added raw_fmt for dates
# - Added raw_unk for dates
# - The algorithm for CMENRTPT in spec is hardcode_ct, but in the program assign_ct
# is used. I kept the algorithm as hardcode_ct in the spec.
# - I consolidated target_hardcoded_value and target_term_value into target_value
# - In the spec we have cm_raw_data and I used it in the template as well
# - Changed is.numeric/is.character into is_numeric/is_character
# - I did not generate id_vars since the default values was enough, certainly we
# can add it later when the customized id_vars are needed.
# - Some extra manipulations done in get_domain_spec()
# - Moved VSTESTCD before qualifired, e.g. VSSTAT, VSPOS so that topic is created first
# - Changed mapping_is_dataset to TRUE for VSTPT, VSDTC
# - Added a new column topic, showing to which topic the mapping belongs to
# - Some code list codes were populated in target_sdtm_variable_controlled_terms_or_format
# I moved them under target_sdtm_variable_codelist_code, e.g. VSPOS, VSLOC

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
#' \dontrun{
#' # VS domain ----
#' spec <- read_spec("vs_sdtm_oak_spec.csv")
#' domain <- "vs"
#'
#' spec <- spec |>
#'   dplyr::filter(
#'     !is.na(target_sdtm_variable),
#'     !is.na(mapping_algorithm),
#'     !mapping_is_dataset %in% c("TRUE")
#'   )
#'
#' old_width <- options(width = 20)
#' generate_code(spec, domain)
#' # Restore original width
#' options(width = old_width$width)
#'
#' # CM domain ----
#'
#' spec <- read_spec("cm_sdtm_oak_spec_cdash.csv")
#'
#' spec <- spec |>
#'   dplyr::filter(
#'     !is.na(target_sdtm_variable),
#'     !is.na(mapping_algorithm),
#'     !mapping_is_dataset %in% c("TRUE")
#'   )
#'
#' domain <- "cm"
#' generate_code(spec, domain)
#'
#' # One can use option width to control the width of the code
#' # Twenty will almost always place every parameter on a separate line
#' old_width <- options(width = 20)
#' generate_code(spec, domain)
#' # Restore original width
#' options(width = old_width$width)
#' }
#'
generate_code <- function(spec, domain, out_dir = ".") {
  admiraldev::assert_data_frame(spec, required_vars = rlang::syms(expected_columns()))
  admiraldev::assert_character_scalar(domain)

  spec_domain <- get_domain_spec(spec, domain)

  topics <- unique(spec_domain$topic)

  styled_code <- purrr::map(
    topics,
    generate_one_topic_code,
    domain = domain,
    spec = spec_domain
  ) |>
    style_the_code()

  file_name <- paste0(domain, "_sdtm_oak_code.R")
  writeLines(styled_code, file.path(out_dir, file_name))
}

style_the_code <- function(code_by_topics) {

  admiraldev::assert_list_of(code_by_topics, "character")

  one_topic <- identical(length(code_by_topics), 1L)

  # TODO
  # - dynamically select the templates based on domain
  if (one_topic) {
    styled_code <- code_by_topics |>
      unlist() |>
      append(cm_template_prefix, after = 0L) |>
      append(cm_template_suffix) |>
      styler::style_text()

    return(styled_code)
  }

  code_by_topics |>
    purrr::map(remove_last_pipe) |>
    unlist() |>
    append(vs_template_prefix, after = 0L) |>
    append(vs_template_suffix) |>
    styler::style_text()
}

#' Generate the code for one topic
#'
#' @param topic The topic to generate the code for.
#' @param domain The SDTM domain.
#' @param spec The specification data frame.
#'
#' @return The code for the topic as a string.
#' @keywords internal
#'
generate_one_topic_code <- function(topic, domain, spec) {
  admiraldev::assert_character_scalar(topic)
  admiraldev::assert_character_scalar(domain)
  admiraldev::assert_data_frame(spec, required_vars = rlang::syms(expected_columns_min()))

  spec_topic <- spec |>
    dplyr::filter(topic %in% {{ topic }})

  domain_topic <- paste(domain, topic, sep = "_") |>
    tolower()

  map_topic <- paste0("\n\n# Map topic ", domain_topic, " ----\n")
  assign_to_domain_topic <- paste0(domain_topic, " <-")

  # Generate the code for each variable row in spec
  spec_topic |>
    dplyr::rowwise() |>
    dplyr::mutate(
      algorithm_code = list(generate_one_var_code(dplyr::pick(dplyr::everything()))),
      .keep = "none"
    ) |>
    unlist() |>
    append(assign_to_domain_topic, after = 0L) |>
    append(map_topic, after = 0L)
}

#' Generate the code for one variable
#'
#' @param spec The specification data frame.
#' @param last_var Logical indicating if this is the last variable in the domain.
#'
#' @return The code for the variable as a string.
#' @keywords internal
#'
generate_one_var_code <- function(spec) {
  admiraldev::assert_data_frame(spec, required_vars = rlang::syms(expected_columns_min()))
  assertthat::assert_that(identical(nrow(spec), 1L))

  args <- list(
    raw_dat = rlang::parse_expr(spec$raw_dataset),
    raw_var = spec$raw_variable,
    tgt_var = spec$target_sdtm_variable,
    tgt_val = spec$target_value,
    ct_spec = rlang::parse_expr("study_ct"),
    ct_clst = spec$target_sdtm_variable_codelist_code,
    raw_fmt = spec$raw_fmt,
    raw_unk = parse_into_c_call(spec$raw_unk)
  )

  # If the ct_clst is missing, then we must remove ct_spec
  if (is.na(args$ct_clst)) {
    args$ct_spec <- NA
  }

  # Remove the arguments that are missing
  args <- purrr::discard(args, \(x) is.vector(x) && anyNA(x))

  # Generate the function call
  generated_call <- rlang::call2(
    spec$mapping_algorithm,
    !!!args
  )

  rlang::expr_deparse(generated_call) |>
    add_pipe()
}

#' This function converts comma separated string into a character vector
#'
#' @param string A string with comma separated values.
#'
#' @return A character vector.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' str_in <- "a, b, c"
#' parse_into_c_call("a, b, c")
#'
#' str_in <- NA_character_
#' parse_into_c_call(str_in)
#' }
#'
parse_into_c_call <- function(str_in) {
  admiraldev::assert_character_scalar(str_in)

  str_out <- str_in |>
    stringr::str_split(stringr::fixed(",")) |>
    unlist() |>
    stringr::str_trim()

  if (all(is.na(str_out))) {
    return(NA)
  }

  rlang::call2("c", !!!str_out)
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
    stringr::str_remove(stringr::fixed("%>%"))

  code_blocks
}

#' Get the specification for a domain and modify it
#'
#' @param spec The specification data frame.
#' @param domain The SDTM domain to get the specification for.
#'
#' @return A tibble with the specification for the domain.
#' @keywords internal
#'
get_domain_spec <- function(spec, domain) {

  admiraldev::assert_data_frame(spec, required_vars = rlang::syms(expected_columns_min()))
  admiraldev::assert_character_scalar(domain)

  # For now assuming that there is only one topic and the topic is the first one

  spec |>
    dplyr::filter(tolower(target_sdtm_domain) %in% tolower(domain)) |>
    dplyr::select(dplyr::all_of(expected_columns_min())) |>
    # For now swapping entity_sub_algorithm with mapping_algorithm since the
    # algorithms like assign_no_ct are the mapping_algorithm and they are populated
    # in the entity_sub_algorithm
    dplyr::mutate(
      entity_sub_algorithm_temp = dplyr::if_else(
        mapping_algorithm %in% "condition_add",
        mapping_algorithm,
        entity_sub_algorithm
      ),
      mapping_algorithm = dplyr::if_else(
        mapping_algorithm %in% "condition_add",
        entity_sub_algorithm,
        mapping_algorithm
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

#' Check if a variable is character
#'
#' @param var_in The variable to check.
#'
#' @return Logical indicating if the variable is character.
#' @export
#'
is_numeric <- function(var_in) {
  grepl(r"{^-?\d*(\.\d+)?(e[+-]?\d+)?$}", var_in)
}

#' Check if a variable is character
#'
#' @param var_in The variable to check.
#'
#' @return Logical indicating if the variable is character.
#' @export
#'
is_character <- function(var_in) {
  grepl("[^0-9eE.-]", var_in)
}

#' Read the specification file
#'
#' @param file The path to the specification file.
#'
#' @returns A tibble with the specification.
#' @export
#'
#' @examples
#' \dontrun{
#' file <- "cm_sdtm_oak_spec_cdash.csv"
#' observed <- read_spec(file)
#' }
#'
read_spec <- function(file) {
  admiraldev::assert_character_scalar(file)

  spec <- utils::read.csv(file = file, na.strings = c("NA", ""), colClasses = "character") |>
    tibble::as_tibble()

  admiraldev::assert_data_frame(spec, required_vars = rlang::syms(expected_columns()))

  return(spec)
}

#' Expected columns in the specification for the domain
#'
#' @keywords internal
#' @noRd
expected_columns_min <- function() {
  c(
    "raw_dataset",
    "raw_variable",
    "target_sdtm_variable",
    "topic",
    "mapping_algorithm",
    "entity_sub_algorithm",
    "condition_add_raw_dat",
    "target_sdtm_variable_codelist_code",
    "raw_data_format",
    "raw_fmt",
    "raw_unk",
    "target_term_value",
    "target_value"
  )
}

#' Expected columns in the specification file
#'
#' @keywords internal
#' @noRd
expected_columns <- function() {
  c(
    "study_number",
    "raw_dataset",
    "raw_dataset_label",
    "raw_variable",
    "raw_variable_label",
    "raw_variable_ordinal",
    "raw_variable_type",
    "raw_data_format",
    "raw_fmt",
    "raw_unk",
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
    "target_resource_raw_variable",
    "target_value",
    "topic"
  )
}
