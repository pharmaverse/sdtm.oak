% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oak_id_vars.R
\name{contains_oak_id_vars}
\alias{contains_oak_id_vars}
\title{Does a vector contain the raw dataset key variables?}
\usage{
contains_oak_id_vars(x)
}
\arguments{
\item{x}{A character vector.}
}
\value{
A logical scalar value.
}
\description{
\code{\link[=contains_oak_id_vars]{contains_oak_id_vars()}} evaluates whether a character vector \code{x} contains
the raw dataset key variable names, i.e. the so called Oak identifier
variables --- these are defined by the return value of \code{\link[=oak_id_vars]{oak_id_vars()}}.
}
\keyword{internal}
