% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oak_id_vars.R
\name{generate_oak_id_vars}
\alias{generate_oak_id_vars}
\title{A function to generate oak_id_vars}
\usage{
generate_oak_id_vars(raw_dat, pat_var, raw_src)
}
\arguments{
\item{raw_dat}{The raw dataset (dataframe)}

\item{pat_var}{Variable that holds the patient number}

\item{raw_src}{Name of the raw source}
}
\value{
dataframe
}
\description{
A function to generate oak_id_vars
}
\examples{
raw_dataset <-
  tibble::tribble(
    ~patnum, ~MDRAW,
    101L, "BABY ASPIRIN",
    102L, "CORTISPORIN",
    103L, NA_character_,
    104L, "DIPHENHYDRAMINE HCL"
  )

# Generate oak_id_vars
generate_oak_id_vars(
  raw_dat = raw_dataset,
  pat_var = "patnum",
  raw_src = "Concomitant Medication"
)
}
