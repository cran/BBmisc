\name{requirePackages}
\alias{requirePackages}
\title{Requires packages that were only suggested.}
\usage{
  requirePackages(packs, why)
}
\arguments{
  \item{packs}{[\code{character}]\cr Names of packages.}

  \item{why}{[\code{character(1)}]\cr Short string
  explaining why packages are required.}
}
\value{
  [\code{logical}]. Logical vectors describing which
  packages could be loaded.  Same length as \code{packs}.
}
\description{
  Requires packages that were only suggested. If some
  packages could not be loaded the following exception is
  thrown: \dQuote{For <why> please install the following
  packages: <missing packages>}.
}

