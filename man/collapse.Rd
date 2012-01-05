\name{collapse}
\alias{collapse}
\title{Collapse vector to string.}
\usage{
  collapse(x, sep = ",")
}
\arguments{
  \item{x}{[\code{vector}]\cr Vector to collapse.}

  \item{sep}{[\code{character(1)}]\cr Passed to
  \code{collapse} in \code{\link{paste}}.}
}
\value{
  [\code{character(1)}].
}
\description{
  A simple wrapper for \code{paste(x, collapse)}.
}

