#' Are all elements of a list / vector uniquely named? 
#' \code{NA} or \dQuote{} are not allowed as names.
#' @title Are all elements of a list / vector uniquely named?
#' @param x [\code{vector}]\cr
#'   The vector or list. 
#' @return [\code{logical(1)}].
#' @export
isProperlyNamed = function(x) {
  ns = names(x)
  (length(x) == 0) ||
    ! (is.null(ns) || length(x) != length(ns) || any(is.na(ns) | ns == "") || any(duplicated(ns)))
}
