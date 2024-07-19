% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive_blfl.R
\name{dtc_timepart}
\alias{dtc_timepart}
\title{Extract time part from ISO 8601 date/time variable}
\usage{
dtc_timepart(dtc, partial_as_na = TRUE, ignore_seconds = TRUE)
}
\arguments{
\item{dtc}{Character vector containing ISO 8601 date/times.}

\item{partial_as_na}{Logical \code{TRUE} or \code{FALSE} indicating whether
partial times should be set to NA (default is \code{TRUE}).}

\item{ignore_seconds}{Logical \code{TRUE} or \code{FALSE} indicating whether
seconds should be ignored (default is \code{TRUE}).}
}
\value{
Character vector containing ISO 8601 times.
}
\description{
The time part is extracted from an ISO 8601 date/time variable.
By default, partial or missing times are set to NA, and seconds are ignored
and not extracted.
}
\keyword{internal}