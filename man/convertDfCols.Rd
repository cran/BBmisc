\name{convertDfCols}
\alias{convertDfCols}
\title{Converts all character columns in a data.frame to factors}
\usage{
  convertDfCols(df, chars.as.factor = FALSE,
    factors.as.char = FALSE, ints.as.num = FALSE)
}
\arguments{
  \item{df}{[\code{data.frame}]\cr Data frame-}

  \item{chars.as.factor}{[\code{logical(1)}]\cr Should
  characters be converted to factors?  Default is
  \code{FALSE}.}

  \item{factors.as.char}{[\code{logical(1)}]\cr Should
  characters be converted to factors?  Default is
  \code{FALSE}.}

  \item{ints.as.num}{[\code{logical(1)}]\cr Should integers
  be converted to numerics?  Default is \code{FALSE}.}
}
\value{
  [\code{data.frame}].
}
\description{
  Converts all character columns in a data.frame to factors
}

