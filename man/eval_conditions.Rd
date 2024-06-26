% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cnd_df.R
\name{eval_conditions}
\alias{eval_conditions}
\title{Evaluate conditions}
\usage{
eval_conditions(dat, ..., .na = NA, .env = rlang::caller_env())
}
\arguments{
\item{dat}{A data frame}

\item{...}{A set of logical conditions, e.g. \verb{y & z, x | z} (\code{x}, \code{y}, \code{z}
would have to exist either as columns in \code{dat} or in the environment
\code{.env}). If multiple expressions are included, they are combined with the
\code{&} operator.}

\item{.na}{Return value to be used when the conditions evaluate to \code{NA}.}

\item{.env}{An optional environment to look for variables involved in logical
expression passed in \code{...}. A data frame or a list can also be passed that
will be coerced to an environment internally.}
}
\value{
A logical vector reflecting matching rows in \code{dat}.
}
\description{
\code{\link[=eval_conditions]{eval_conditions()}} evaluates a set of conditions in the context of a
data frame and an optional environment.

The utility of this function is to provide an easy way to generate a logical
vector of matching records from a set of logical conditions involving
variables in a data frame (\code{dat}) and optionally in a supplementary
environment (\code{.env}). The set of logical conditions are provided as
expressions to be evaluated in the context of \code{dat} and \code{.env}.

Variables are looked up in \code{dat}, then in \code{.env}, then in the calling
function's environment, followed by its parent environments.
}
\examples{
# Create a sample data frame
df <- data.frame(
  x = c(1, 2, NA_integer_, 4, 5),
  y = c(TRUE, FALSE, TRUE, FALSE, TRUE),
  z = c("a", "b", "a", "b", "a")
)

# Simple condition on one column
sdtm.oak:::eval_conditions(df, x > 2)

# Combined conditions on multiple columns
sdtm.oak:::eval_conditions(df, x > 2 & y)
sdtm.oak:::eval_conditions(df, x > 2, y)

# Using conditions with NA handling
df_with_na <- data.frame(
  x = c(1, 2, NA, 4, 5),
  y = c(TRUE, FALSE, TRUE, FALSE, TRUE)
)
sdtm.oak:::eval_conditions(df_with_na, x > 2, .na = FALSE)

# The environment where `eval_conditions()` is called is also inspected
# when evaluating conditions in `...`.
w <- 1
sdtm.oak:::eval_conditions(df, x > w)

# Using an environment
env <- rlang::env(w = 2)
sdtm.oak:::eval_conditions(df, x > w, .env = env)

# In place of an environment, you may alternatively pass a list or data frame.
sdtm.oak:::eval_conditions(df, x > w, .env = list(w = 3))
sdtm.oak:::eval_conditions(df, x > w, .env = tibble::tibble(w = 4))

}
\keyword{internal}
