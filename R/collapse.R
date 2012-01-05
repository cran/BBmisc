#' A simple wrapper for \code{paste(x, collapse)}.
#' @title Collapse vector to string.
#' @param x [\code{vector}]\cr
#'   Vector to collapse.
#' @param sep [\code{character(1)}]\cr
#'   Passed to \code{collapse} in  \code{\link{paste}}.
#' @return [\code{character(1)}].
#' @export
collapse = function(x, sep=",") {
  paste(x, collapse=sep)
}
