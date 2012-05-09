\name{parallelStart}
\alias{parallelStart}
\title{Parallelization setup for parallelMap.}
\usage{
  parallelStart(mode = "local", cpus, ...,
    level = as.character(NA), log = NULL)
}
\arguments{
  \item{mode}{[\code{character(1)}]\cr Which parallel mode
  should be used: \dQuote{local}, \dQuote{multicore},
  \dQuote{snowfall}.  Default is \dQuote{local} without
  parallel execution.}

  \item{cpus}{[\code{integer(1)}]\cr Number of used cpus.
  Default is \code{\link[Rmpi]{mpi.universe.size}} for
  snowfall/MPI and 1 otherwise.}

  \item{...}{[any]\cr Optional parameters, only passed to
  \code{\link[snowfall]{sfInit}} currently.}

  \item{level}{[\code{character(1)}]\cr You can set this so
  only calls to \code{\link{parallelMap}} are parallelized
  that have the same level specified.  Default is \code{NA}
  which means all calls are parallelized.}

  \item{log}{[\code{character(1)}]\cr Path to an existing
  directory where a log files for each job is stored via
  \code{\link{sink}}. Note that all nodes must have write
  access to exactly this path.  Files are named
  "<iteration_number>.log".  \code{NULL} means no logging
  and this is the default.}
}
\value{
  Nothing.
}
\description{
  Defines the underlying parallelization mode (currently
  multicore or snowfall) for \code{\link{parallelMap}} and
  allows to set a \dQuote{level} of parallelization. Only
  calls to \code{\link{parallelMap}} with a matching level
  are parallelized.
}
\details{
  For snowfall \code{\link[snowfall]{sfStop}},
  \code{\link[snowfall]{sfSetMaxCPUs}},
  \code{\link[snowfall]{sfInit}},
  \code{\link[snowfall]{sfClusterSetupRNG}} are called in
  this order.
}

