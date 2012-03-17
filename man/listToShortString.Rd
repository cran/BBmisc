\name{listToShortString}
\alias{listToShortString}
\title{Converts list to short string describing contents.}
\usage{
  listToShortString(xs)
}
\arguments{
  \item{xs}{[\code{list}]\cr The list.}
}
\value{
  [\code{character(1)}].
}
\description{
  Converts list to short string describing contents. Looks
  like this \dQuote{a=1,<unamed>=2,b=<data.frame>}. Vectors
  are displayed till a certain length.
}
\examples{
listToShortString(list(a=1, b=NULL, "foo", c=1:10))
}

