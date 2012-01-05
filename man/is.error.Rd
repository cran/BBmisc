\name{is.error}
\alias{is.error}
\title{Is return value of try an exception?}
\usage{
  is.error(x)
}
\arguments{
  \item{x}{[any]\cr Return value of a
  \code{\link{try}}-statement.}
}
\value{
  [\code{logical(1)}].
}
\description{
  A simple wrapper for \code{is(x, "try-error")}.
}

