\name{parallelExport}
\alias{parallelExport}
\title{Export a larger object which is needed in slave code of \code{\link{parallelMap}}.}
\usage{
  parallelExport(..., list = character(0))
}
\arguments{
  \item{...}{[\code{character(1)}]\cr Names of object to
  export.}

  \item{list}{[list of \code{character(1)}]\cr Names of
  objects to export.  Alternative way to pass arguments.}
}
\value{
  Nothing.
}
\description{
  Objects can later be retrieved with
  \code{\link{parallelGetExported}} in slave code.
}
\details{
  For local and multicore mode the objects are stored in a
  package environment, for snowfall
  \code{\link[snowfall]{sfExport}} is used internally.
}
\examples{
foo <- 100
f <- function(x) x + parallelGetExported("foo")
parallelStart(mode="local")
parallelExport("foo")
y <- parallelMap(f, 1:3)
parallelStop()
}

