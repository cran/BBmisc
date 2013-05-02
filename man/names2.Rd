\name{names2}
\alias{names2}
\title{Replacement for names which always returns a vector.}
\usage{
  names2(x)
}
\arguments{
  \item{x}{Object, probably named.}
}
\value{
  [\code{character}]: vector of the same length as
  \code{x}.
}
\description{
  A simple wrapper for \code{\link[base]{names}}. Returns a
  vector even if no names attribute is set (and not
  \code{NULL}). Missing names are returned as \code{NA},
  not \code{""}.
}
\examples{
x <- 1:3
names(x)
names2(x)
names(x[1:2]) = letters[1:2]
names(x)
names2(x)
}

