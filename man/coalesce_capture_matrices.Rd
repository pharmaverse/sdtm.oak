% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtc_utils.R
\name{coalesce_capture_matrices}
\alias{coalesce_capture_matrices}
\title{Coalesce capture matrices}
\usage{
coalesce_capture_matrices(...)
}
\arguments{
\item{...}{A sequence of capture matrices.}
}
\value{
A single capture matrix whose values have been coalesced in the
sense of \link[dplyr:coalesce]{coalesce()}.
}
\description{
\code{\link[=coalesce_capture_matrices]{coalesce_capture_matrices()}} combines several capture matrices into one.
Each argument of \code{...} should be a capture matrix in the sense of the output
by \code{\link[=complete_capture_matrix]{complete_capture_matrix()}}, meaning a character matrix of six columns
whose names are: \code{year}, \code{mon}, \code{mday}, \code{hour}, \code{min} or \code{sec}.
}
\keyword{internal}
