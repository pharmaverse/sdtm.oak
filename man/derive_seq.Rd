% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive_seq.R
\name{derive_seq}
\alias{derive_seq}
\title{Derive the sequence number (\code{--SEQ}) variable}
\usage{
derive_seq(
  tgt_dat,
  tgt_var,
  rec_vars,
  sbj_vars = sdtm.oak::sbj_vars(),
  start_at = 1L
)
}
\arguments{
\item{tgt_dat}{The target dataset, a data frame.}

\item{tgt_var}{The target SDTM variable: a single string indicating the name
of the sequence number (\code{--SEQ}) variable, e.g. \code{"DSSEQ"}. Note that
supplying a name not ending in \code{"SEQ"} will raise a warning.}

\item{rec_vars}{A character vector of record-level identifier variables.}

\item{sbj_vars}{A character vector of subject-level identifier variables.}

\item{start_at}{The sequence numbering starts at this value (default is \code{1}).}
}
\value{
Returns the data frame supplied in \code{tgt_dat} with the newly derived
variable, i.e. the sequence number (\code{--SEQ}), whose name is that passed in
\code{tgt_var}. This variable is of type integer.
}
\description{
\code{\link[=derive_seq]{derive_seq()}} creates a new identifier variable: the sequence number
(\code{--SEQ}).

This function adds a newly derived variable to \code{tgt_dat}, namely the sequence
number (\code{--SEQ}) whose name is the one provided in \code{tgt_var}. An integer
sequence is generated that uniquely identifies each record within the domain.

Prior to the derivation of \code{tgt_var}, the data frame \code{tgt_dat} is sorted
according to grouping variables indicated in \code{rec_vars}.
}
\examples{
# A VS raw data set example
(vs <- read_domain_example("vs"))

# Derivation of VSSEQ
rec_vars <- c("STUDYID", "USUBJID", "VSTESTCD", "VSDTC", "VSTPTNUM")
derive_seq(tgt_dat = vs, tgt_var = "VSSEQ", rec_vars = rec_vars)

# An APSC raw data set example
(apsc <- read_domain_example("apsc"))

# Derivation of APSEQ
derive_seq(
  tgt_dat = apsc,
  tgt_var = "APSEQ",
  rec_vars = c("STUDYID", "RSUBJID", "SCTESTCD"),
  sbj_vars = c("STUDYID", "RSUBJID")
)
}
