\name{warningf}
\alias{warningf}
\title{Wrapper for warning and sprintf.}
\usage{
  warningf(...)
}
\arguments{
  \item{...}{[any]\cr See \code{\link{sprintf}}.}
}
\value{
  Nothing.
}
\description{
  A wrapper for \code{\link{warning}} with
  \code{\link{sprintf}} applied to the arguments.
}
\examples{
msg <- "a warning"
warningf("this is \%s", msg)
}

