\name{convertInteger}
\alias{convertInteger}
\title{Conversion for single integer.}
\usage{
  convertInteger(x)
}
\arguments{
  \item{x}{[any]\cr Argument.}
}
\value{
  Either a single integer if conversion was done or
  \code{x} unchanged.
}
\description{
  Convert single numeric to integer only if the numeric
  represents a single integer, e.g. 1 to 1L. Otherwise the
  argument is returned unchanged.
}

