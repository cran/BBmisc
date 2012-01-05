\name{convertIntegers}
\alias{convertIntegers}
\title{Conversion for integer vector.}
\usage{
  convertIntegers(x)
}
\arguments{
  \item{x}{[any]\cr Argument.}
}
\value{
  Either an integer vector if conversion was done or
  \code{x} unchanged.
}
\description{
  Convert numeric vector to integer vector if the numeric
  vector fully represents an integer vector, e.g.
  \code{c(1, 5)} to \code{c(1L, 5L)}. Otherwise the
  argument is returned unchanged.
}

