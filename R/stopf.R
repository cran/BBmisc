#' A simple wrapper for \code{stop(sprintf(...))}.
#' @title Wrapper for stop and sprintf.
#' @param ... [any]\cr
#'   See \code{\link{sprintf}}.
#' @return Nothing.
#' @export
stopf = function(...) {
  msg = sprintf(...)
  .Internal(stop(TRUE, msg))
}
