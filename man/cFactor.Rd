\name{cFactor}
\alias{cFactor}
\title{Combine multiple factors and return a factor.}
\usage{
  cFactor(...)
}
\arguments{
  \item{...}{[\code{factor}]\cr The factors.}
}
\value{
  [\code{factor}].
}
\description{
  Combine multiple factors and return a factor. Note that
  function does not inherit from \code{\link{c}} to not
  change R semantics behind your back when this package is
  loaded.
}

