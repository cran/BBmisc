\name{splitTime}
\alias{splitTime}
\title{Split seconds into handy chunks of time.}
\usage{
  splitTime(seconds,
    unit = c("years", "days", "hours", "minutes", "seconds"))
}
\arguments{
  \item{seconds}{[\code{numeric(1)}]\cr Number of seconds.
  If not an integer, it is rounded down.}

  \item{unit}{[\code{character(1)}]\cr Largest unit to
  split seconds into. Must be one of the ones listed in the
  function signature.}
}
\value{
  [\code{numeric(5)}]. A named vector containing the
  \dQuote{years}, \dQuote{days}, \dQuote{hours},
  \dQuote{minutes} and \dQuote{seconds}. Units larger than
  the given \code{unit} are \code{NA}.
}
\description{
  Note that a year is simply defined as exactly 365 days.
}
\examples{
splitTime(1000)
}

