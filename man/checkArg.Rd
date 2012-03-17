\name{checkArg}
\alias{checkArg}
\title{Check for a function argument.}
\usage{
  checkArg(x, cl, len, min.len, choices, subset,
    lower = NA, upper = NA, na.ok = TRUE, formals)
}
\arguments{
  \item{x}{[any]\cr Argument.}

  \item{cl}{[\code{character(1)}]\cr Class that argument
  must have. Checked with \code{is}.}

  \item{len}{[\code{integer(1)}]\cr Length that argument
  must have.  Not checked if not passed, which is the
  default.}

  \item{min.len}{[\code{integer(1)}]\cr Minimal length that
  argument must have.  Not checked if not passed, which is
  the default.}

  \item{choices}{[any]\cr Discrete number of choices,
  expressed by a vector of R objects.  If passed, argument
  must be identical to one of these and nothing else is
  checked.}

  \item{subset}{[any]\cr Discrete number of choices,
  expressed by a vector of R objects.  If passed, argument
  must be identical to a subset of these and nothing else
  is checked.}

  \item{lower}{[\code{numeric(1)}]\cr Lower bound for
  numeric vector arguments.  Default is \code{NA}, which
  means not required.}

  \item{upper}{[\code{numeric(1)}]\cr Upper bound for
  numeric vector arguments.  Default is \code{NA}, which
  means not required.}

  \item{na.ok}{[\code{logical(1)}]\cr Is it ok if a vector
  argument contains NAs?  Default is \code{TRUE}.}

  \item{formals}{[\code{character}]\cr Is it ok if a vector
  argument contains NAs?  Default is \code{TRUE}.}
}
\value{
  Nothing.
}
\description{
  Check for a function argument. Throws exception if checks
  are not passed. Note that argument is evaluated when
  checked.
}
\examples{
x <- 1L
checkArg(x, "integer", len=1, na.ok=FALSE, upper=3L)
x <- as.integer(NA)
checkArg(x, "integer", len=1, na.ok=TRUE)
x <- c("foo", "bar")
checkArg(x, "character")
x <- "foo"
checkArg(x, choices=c("foo", "bar"))
x <- c("foo", "bar")
checkArg(x, subset=c("foo", "bar"))
fun <- function(foo, bar)
checkArg(fun, formals=c("foo", "bar"))
}

