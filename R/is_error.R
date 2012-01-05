#' A simple wrapper for \code{is(x, "try-error")}.
#' @title Is return value of try an exception?
#' @param x [any]\cr
#'   Return value of a \code{\link{try}}-statement.
#' @return [\code{logical(1)}].
#' @export
is.error = function(x) {
  inherits(x, "try-error")
}
