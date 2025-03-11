#' Function to generate supplemental domains SUPP--
#'
#' @param sdtm_dataset SDTM output used to split supplemental domains.
#' @param idvar Variable name used to for variable.
#' @param spec Data frame of specifications..
#' @param eval Character string of the evaluator.
#'
#' @return Supplemental domains.
#' @export
#'
#' @examples
#' path <- here::here("inst")
#' dm <- readRDS(paste0(path, "/domain/dm.rds"))
#' spec <- read.csv(paste0(path, "/spec/spec.csv"))
#' suppdm <- gen_supp(dm, idvar = NULL, spec = spec, eval = "CLINICAL STUDY SPONSOR")
gen_supp <-
  function(sdtm_dataset = NULL,
           idvar = NULL,
           spec = NULL,
           eval = NA_character_ ### Should this be 1 single character value or a vector?
  ) {
  admiraldev::assert_character_scalar(eval)
  admiraldev::assert_data_frame(spec, required_vars = rlang::syms(c("Domain", "Variable", "Label", "Origin")))

  # Create vectors for later use
  domain <- unique(sdtm_dataset$DOMAIN)
  assertthat::assert_that(identical(length(domain), 1L),
    msg = "There are multiple domain names in the SDTM dataset"
  )

  assertthat::assert_that(domain %in% toupper(unique(spec$Domain)),
                          msg = paste("Data specifications for", domain, "are not provided")
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

  supp_cols <- spec$Variable
  admiraldev::assert_data_frame(sdtm_dataset, required_vars = rlang::syms(c(idvar, supp_cols, "STUDYID", "DOMAIN", "USUBJID")))

  supp <- sdtm_dataset |>
    tidyr::pivot_longer(
      cols = dplyr::any_of(supp_cols),
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

        return(names_values)
      },
      values_to = "IDVARVAL"
    ) |>
    dplyr::left_join(spec, by = c("QNAM" = "Variable", "DOMAIN" = "Domain")) |>
    dplyr::mutate(
      RDOMAIN = DOMAIN,
      QORIG = Origin,
      QLABEL = Label,
      QEVAL = toupper(eval)
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

  return(supp)
}
