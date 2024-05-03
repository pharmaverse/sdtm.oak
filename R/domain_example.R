#' Find the path to an example SDTM domain file
#'
#' @description
#' [domain_example()] resolves the local path to a SDTM domain example file. The
#' domain examples files were imported from
#' [pharmaversesdtm](https://cran.r-project.org/package=pharmaversesdtm). See
#' Details section for available datasets.
#'
#' @details
#' Datasets were obtained from
#' [pharmaversesdtm](https://cran.r-project.org/package=pharmaversesdtm) but are
#' originally sourced from the [CDISC pilot
#' project](https://github.com/cdisc-org/sdtm-adam-pilot-project) or have been
#' constructed ad-hoc by the
#' [admiral](https://cran.r-project.org/package=admiral) team. These datasets
#' are bundled with `{sdtm.oak}`, thus obviating a dependence on
#' `{pharmaversesdtm}`.
#'
#' ### Example SDTM domains
#'
#' \describe{
#' \item{`"ae_ophtha"`}{Ophthalmology Adverse Events Dataset.}
#' \item{`"ae"`}{Adverse Events Dataset-updated.}
#' \item{`"ce_vaccine"`}{Clinical Events Dataset for Vaccine Studies.}
#' \item{`"cm"`}{Concomitant Medication Dataset.}
#' \item{`"dm_vaccine"`}{Demographics Dataset for Vaccine Studies.}
#' \item{`"dm"`}{Demography Dataset.}
#' \item{`"ds"`}{Disposition Dataset-updated.}
#' \item{`"eg"`}{	Electrocardiogram Dataset.}
#' \item{`"ex_ophtha"`}{Ophthalmology Exposure Dataset.}
#' \item{`"ex_vaccine"`}{Exposures Dataset for Vaccine Studies.}
#' \item{`"ex"`}{Exposure Dataset.}
#' \item{`"face_vaccine"`}{Findings About Clinical Events Dataset for Vaccine Studies.}
#' \item{`"is_vaccine"`}{Immunogenicity Specimen Assessments Dataset for Vaccine Studies.}
#' \item{`"lb"`}{Laboratory Measurements Dataset.}
#' \item{`"mh"`}{Medical History Dataset-updated.}
#' \item{`"oe_ophtha"`}{Ophthalmology Adverse Events Dataset.}
#' \item{`"pc"`}{Pharmacokinetics Concentrations Dataset.}
#' \item{`"pp"`}{Pharmacokinetic Parameters Dataset.}
#' \item{`"qs_ophtha"`}{Ophthalmology Questionnaire Dataset.}
#' \item{`"rs_onco_irecist"`}{Disease Response Dataset (iRECIST).}
#' \item{`"rs_onco"`}{Disease Response Dataset.}
#' }
#'
#' @param example A string with either the basename, file name, or relative path
#'   to a SDTM domain example file bundled with `{stdm.oak}`, e.g. `"cm"`
#'   (Concomitant Medication) or `"ae"` (Adverse Events).
#'
#' @returns The local path to an example file if `example` is supplied, or a
#'   character vector of example file names.
#'
#' @examples
#' # If no example is provided it returns a vector of possible choices.
#' domain_example()
#'
#' # Get the local path to the Concomitant Medication dataset file.
#' domain_example("cm")
#'
#' Local path to the Adverse Events dataset file.
#' domain_example("ae")
#'
#' @source See \url{https://cran.r-project.org/package=pharmaversesdtm}.
#'
#' @seealso [read_domain_example()]
#' @export
domain_example <- function(example) {
  # If no example is requested, then return all available files.
  if (missing(example)) {
    domain_path <- system.file("domain", package = "sdtm.oak", mustWork = TRUE)
    domain_files <- list.files(domain_path, pattern = "*.rds")
    domains <- tools::file_path_sans_ext(basename(domain_files))
    return(domains)
  }

  # Otherwise, resolve the local path to the example requested.
  admiraldev::assert_character_scalar(example, optional = TRUE)
  base_name <- tools::file_path_sans_ext(basename(example))
  path <- file.path("domain", paste0(base_name, ".rds"))
  local_path <- system.file(path, package = "sdtm.oak")

  if (identical(local_path, "")) {
    stop(
      glue::glue(
        "'{example}' does not match any domain example files. Run `domain_example()` for options."
      ),
      call. = FALSE
    )
  } else {
    local_path <-
      system.file(path, package = "sdtm.oak", mustWork = TRUE)
    return(local_path)
  }
}

#' Read an example SDTM domain
#'
#' [read_domain_example()] imports one of the bundled SDTM domain examples
#'  as a [tibble][tibble::tibble-package] into R. See [domain_example()] for
#'  possible choices.
#'
#' @param example The name of SDTM domain example, e.g. `"cm"` (Concomitant
#'   Medication) or `"ae"` (Adverse Events). Run `read_domain_example()` for
#'   available example files.
#'
#' @returns A [tibble][tibble::tibble-package] with an SDTM domain dataset, or a
#'   character vector of example file names.
#'
#' @examples
#' # Leave the `example` parameter as missing for available example files.
#' read_domain_example()
#'
#' # Read the example Concomitant Medication domain.
#' read_domain_example("cm")
#'
#' # Read the example Adverse Events domain.
#' read_domain_example("ae")
#'
#' @seealso [domain_example()]
#' @export
read_domain_example <- function(example) {
  if (missing(example)) {
    return(domain_example())
  } else {
    admiraldev::assert_character_scalar(example)
  }

  path <- domain_example(example)
  readr::read_rds(file = path)
}
