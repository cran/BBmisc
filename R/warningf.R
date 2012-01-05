#' A simple wrapper for \code{warning(sprintf(...))}.
#' @title Wrapper for warning and sprintf.
#' @param ... [any]\cr
#'   See \code{\link{sprintf}}.
#' @return Nothing.
#' @export
warningf = function(...) {
  msg = sprintf(...)
  .Internal(warning(TRUE, FALSE, msg))
}
