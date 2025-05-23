% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_sdtm_supp.R
\name{generate_sdtm_supp}
\alias{generate_sdtm_supp}
\title{Function to generate final SDTM domain and supplemental domain SUPP--}
\usage{
generate_sdtm_supp(
  sdtm_dataset,
  idvar = NULL,
  supp_qual_info,
  qnam_var,
  label_var,
  orig_var
)
}
\arguments{
\item{sdtm_dataset}{SDTM output used to split supplemental domains.}

\item{idvar}{Variable name for IDVAR variable.}

\item{supp_qual_info}{User-defined data frame of specifications for suppquals
which contains \code{qnam_var}, \code{label_var} and \code{orig_var}.}

\item{qnam_var}{Variable name in user-defined \code{supp_qual_info} for QNAM variable.}

\item{label_var}{Variable name in user-defined \code{supp_qual_info} for QLABEL variable.}

\item{orig_var}{Variable name in user-defined \code{supp_qual_info} for QORIG variable.}
}
\value{
List of SDTM domain with suppquals dropped and corresponding supplemental domain.
}
\description{
Function to generate final SDTM domain and supplemental domain SUPP--
}
\examples{
dm <- read_domain_example("dm")
supp_qual_info <- read.csv(system.file("spec/suppqual_spec.csv", package = "sdtm.oak"))

dm_suppdm <-
  generate_sdtm_supp(
    dm,
    idvar = NULL,
    supp_qual_info = supp_qual_info,
    qnam_var = "Variable",
    label_var = "Label",
    orig_var = "Origin"
  )
}
