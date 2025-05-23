% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ct.R
\name{inform_on_ct_mappability}
\alias{inform_on_ct_mappability}
\title{Inform on the mappability of terms to controlled terminology}
\usage{
inform_on_ct_mappability(x, from)
}
\arguments{
\item{x}{A character vector of terms to be checked.}

\item{from}{A character vector of valid controlled terminology terms.}
}
\value{
Invisibly returns \code{TRUE} if all terms are mappable; otherwise,
prints an informative message and returns \code{FALSE} invisibly.
}
\description{
\code{\link[=inform_on_ct_mappability]{inform_on_ct_mappability()}} checks whether all values in \code{x} can be mapped
using the controlled terminology terms in \code{from}. It raises an informative
message if any values in \code{x} are not mappable.
}
\keyword{internal}
