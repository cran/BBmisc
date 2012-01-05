#' A simple wrapper for \code{message(sprintf(...))}.
#' @title Wrapper for message and sprintf.
#' @param ... [any]\cr
#'   See \code{\link{sprintf}}.
#' @return Nothing.
#' @export
messagef = function(...) {
  message(sprintf(...))
}
