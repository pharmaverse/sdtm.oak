% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtc_utils.R
\name{assert_dtc_format}
\alias{assert_dtc_format}
\title{Assert dtc format}
\usage{
assert_dtc_format(.format)
}
\arguments{
\item{.format}{The argument of \code{\link[=create_iso8601]{create_iso8601()}}'s \code{.format} parameter.}
}
\value{
This function throws an error if \code{.format} is not either:
\itemize{
\item A character vector of formats permitted by \code{\link[=assert_dtc_fmt]{assert_dtc_fmt()}};
\item A list of character vectors of formats permitted by \code{\link[=assert_dtc_fmt]{assert_dtc_fmt()}}.
}

Otherwise, it returns \code{.format} invisibly.
}
\description{
\code{\link[=assert_dtc_format]{assert_dtc_format()}} is an internal helper function aiding with the checking
of the \code{.format} parameter of \code{\link[=create_iso8601]{create_iso8601()}}.
}
\keyword{internal}
