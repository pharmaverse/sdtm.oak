% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_dttm_fmt.R
\name{fmt_cmp}
\alias{fmt_cmp}
\title{Regexps for date/time format components}
\usage{
fmt_cmp(
  sec = "S+",
  min = "M+",
  hour = "H+",
  mday = "d+",
  mon = "m+",
  year = "y+"
)
}
\arguments{
\item{sec}{A string pattern for matching the second format component.}

\item{min}{A string pattern for matching the minute format component.}

\item{hour}{A string pattern for matching the hour format component.}

\item{mday}{A string pattern for matching the month day format component.}

\item{mon}{A string pattern for matching the month format component.}

\item{year}{A string pattern for matching the year format component.}
}
\value{
A named character vector of date/time format patterns. This a vector
of six elements, one for each date/time component.
}
\description{
\code{\link[=fmt_cmp]{fmt_cmp()}} creates a character vector of patterns to match individual
format date/time components.
}
\examples{
# Regexps to parse format components
fmt_cmp()

fmt_cmp(year = "yyyy")

}
