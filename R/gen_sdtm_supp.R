#' Function to generate final SDTM domain and supplemental domain SUPP--
#'
#' @param sdtm_dataset SDTM output used to split supplemental domains.
#' @param idvar Variable name for IDVAR variable.
#' @param supp_qual_info User-defined data frame of specifications for suppquals
#' which contains `qnam_var`, `label_var` and `orig_var`.
#' @param qnam_var Variable name in user-defined `supp_qual_info` for QNAM variable.
#' @param label_var Variable name in user-defined `supp_qual_info` for QLABEL variable.
#' @param orig_var Variable name in user-defined `supp_qual_info` for QORIG variable.
#'
#' @return List of SDTM domain with suppquals dropped and corresponding supplemental domain.
#' @export
#'
#' @examples
#' dm <- read_domain_example("dm")
#' supp_qual_info <- read.csv(system.file("spec/suppqual_spec.csv", package = "sdtm.oak"))
#' final <-
#'   gen_sdtm_supp(
#'     dm,
#'     idvar = NULL,
#'     supp_qual_info = supp_qual_info,
#'     qnam_var = "Variable",
#'     label_var = "Label",
#'     orig_var = "Origin"
#'   )
gen_sdtm_supp <-
  function(sdtm_dataset,
           idvar = NULL,
           supp_qual_info,
           qnam_var,
           label_var,
           orig_var) {
    admiraldev::assert_character_scalar(qnam_var)
    admiraldev::assert_character_scalar(label_var)
    admiraldev::assert_character_scalar(orig_var)

    admiraldev::assert_data_frame(supp_qual_info, required_vars = rlang::syms(c(qnam_var, label_var, orig_var)))

    # Create vectors for later use
    domain <- unique(sdtm_dataset$DOMAIN)
    assertthat::assert_that(identical(length(domain), 1L),
      msg = "There are multiple domain names in the SDTM dataset"
    )

    # Add dummy SEQ variable for DM domain.
    # The DMSEQ will be removed later.
    if (identical(domain, "DM") && is.null(idvar)) {
      sdtm_dataset$DMSEQ <- NA_integer_
    }

    # The default setting is to use xxSEQ variable for IDVAR
    idvar <- if (is.null(idvar)) {
      paste0(domain, "SEQ")
    }
    admiraldev::assert_character_scalar(idvar)

    # Each supplemental variable should only be mapped to one unique label
    supp_qual_info <- supp_qual_info |>
      dplyr::distinct(dplyr::across(dplyr::all_of(c(qnam_var, label_var, orig_var))), .keep_all = TRUE)

    supp_cols <- supp_qual_info |>
      dplyr::select(dplyr::all_of(qnam_var)) |>
      dplyr::pull()

    admiraldev::assert_data_frame(sdtm_dataset, required_vars = rlang::syms(c(
      idvar, supp_cols, "STUDYID", "DOMAIN", "USUBJID"
    )))

    supp <- sdtm_dataset |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(supp_cols),
        names_to = "QNAM",
        values_to = "QVAL",
        values_transform = as.character
      ) |>
      # Exclude the records where QVAL is null
      dplyr::filter(!is.na(QVAL)) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(idvar),
        names_to = "IDVAR",
        names_transform = function(names_values) {
          # Replace DMSEQ with NA
          names_values <- gsub("^DMSEQ$", NA_character_, names_values)
        },
        values_to = "IDVARVAL"
      ) |>
      dplyr::left_join(supp_qual_info, by = c("QNAM" = qnam_var)) |>
      dplyr::mutate(
        RDOMAIN = DOMAIN,
        QORIG = Origin,
        QLABEL = Label,
        QEVAL = NA_character_
      ) |>
      dplyr::select(
        STUDYID,
        RDOMAIN,
        USUBJID,
        IDVAR,
        IDVARVAL,
        QNAM,
        QLABEL,
        QVAL,
        QORIG,
        QEVAL
      )

    # Assign labels to SUPPQUAL
    labels <-
      c(
        labels <-
          c(
            "Study Identifier",
            "Related Domain Abbreviation",
            "Unique Subject Identifier",
            "Identifying Variable",
            "Identifying Variable Value",
            "Qualifier Variable Name",
            "Qualifier Variable Label",
            "Data Value",
            "Origin",
            "Evaluator"
          )
      )

    for (v in seq_len(length(labels))) {
      attr(supp[[v]], "label") <- labels[v]
    }

    sdtm_output <- sdtm_dataset |>
      dplyr::select(-dplyr::all_of(supp_cols), -dplyr::any_of("DMSEQ"))

    domain <- toupper(domain)
    supp_domain <- paste0("SUPP", domain)
    final <- rlang::list2(
      {{ domain }} := sdtm_output,
      {{ supp_domain }} := supp
    )

    cli::cli_alert_info("QEVAL should be manually programmed in supplemental domain.")

    return(final)
  }
