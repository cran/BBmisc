#' @title Create named list, possibly initialized with a certain element.
#'
#' @description
#' Even an empty list will always be named.
#'
#' @param names [\code{character}]\cr
#'   Names of elements.
#' @param init [valid R expression]\cr
#'   If given all list elements are initialized to this, otherwise
#'   \code{NULL} is used.
#' @return [\code{list}].
#' @export
#' @examples
#' namedList(c("a", "b"))
#' namedList(c("a", "b"), init = 1)
namedList = function(names, init) {
  if (missing(names))
    return(setNames(list(), character(0L)))
  n = length(names)
  if (missing(init))
    xs = vector("list", n)
  else
    xs = replicate(n, init, simplify = FALSE)
  setNames(xs, names)
}
