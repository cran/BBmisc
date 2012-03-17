#' A wrapper for \code{\link{warning}} with \code{\link{sprintf}} applied to the arguments.
#' @title Wrapper for warning and sprintf.
#' @param ... [any]\cr
#'   See \code{\link{sprintf}}.
#' @return Nothing.
#' @export
#' @examples
#' msg <- "a warning"
#' warningf("this is %s", msg)
warningf = function(...) {
  msg = sprintf(...)
  obj = simpleWarning(msg, call=sys.call(sys.parent()))
  warning(obj)
}
