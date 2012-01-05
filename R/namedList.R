#' Create named list, possibly inititialized with 
#' a certain element.
#' @title Create named list, possibly inititialized.
#' @param names [\code{character}]\cr
#'   Names of elements.
#' @param init [valid R expression]\cr
#'   If given all list elements are initialized to this, otherwise
#'   \code{NULL} is used.
#' @return [\code{list}].
#' @export
namedList = function(names, init) {
  if (missing(names))
    return(list())
  n = length(names)
  if (missing(init))
    xs = vector("list", n)
  else
    xs = replicate(n, init, simplify=FALSE)
  names(xs) = names
  return(xs)
}
