% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ct.R
\name{ct_spec_vars}
\alias{ct_spec_vars}
\title{Controlled terminology variables}
\usage{
ct_spec_vars(set = c("all", "ct_clst", "from", "to"))
}
\arguments{
\item{set}{A scalar character (string), one of: \code{"all"} (default), \code{"ct_clst"},
\code{"from"} or \code{"to"}.}
}
\description{
\code{\link[=ct_spec_vars]{ct_spec_vars()}} returns the mandatory variables to be present in a data set
representing a controlled terminology. By default, it returns all required
variables.

If only the subset of variables used for matching terms are needed, then
request this subset of variables by passing the argument value \code{"from"}. If
only the mapping-to variable is to be requested, then simply pass \code{"to"}. If
only the codelist code variable name is needed then pass \code{"ct_clst"}.
}
\keyword{internal}
