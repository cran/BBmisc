% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setValue.R
\name{setValue}
\alias{setValue}
\title{Set a list element to a new value.}
\usage{
setValue(obj, index, newval)
}
\arguments{
\item{obj}{[\code{list}]\cr}

\item{index}{[\code{character} | \code{integer}]\cr
Index or indices where to insert the new values.}

\item{newval}{[any]\cr
Inserted elements(s).
Has to be a list if \code{index} is a vector.}
}
\value{
[\code{list}]
}
\description{
This wrapper supports setting elements to \code{NULL}.
}
