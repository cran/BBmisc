#' Conversion for single integer.
#'
#' Convert single numeric to integer only if the numeric represents a single integer,
#' e.g. 1 to 1L.
#' Otherwise the argument is returned unchanged.
#'
#' @param x [any]\cr
#'   Argument.
#' @return Either a single integer if conversion was done or \code{x} unchanged.
#' @export
#' @examples
#' str(convertInteger(1.0))
#' str(convertInteger(1.3))
#' str(convertInteger(c(1.0, 2.0)))
#' str(convertInteger("foo"))
convertInteger = function(x) {
  if (is.integer(x) || length(x) != 1L)
    return(x)
  if (is.na(x) || (is.numeric(x) && isTRUE(all.equal(x, as.integer(x)))))
    return(as.integer(x))
  return(x)
}

#' Conversion for integer vector.
#'
#' Convert numeric vector to integer vector if the numeric vector fully represents
#' an integer vector,
#' e.g. \code{c(1, 5)} to \code{c(1L, 5L)}.
#' Otherwise the argument is returned unchanged.
#'
#' @param x [any]\cr
#'   Argument.
#' @return Either an integer vector if conversion was done or \code{x} unchanged.
#' @export
#' @examples
#' str(convertIntegers(1.0))
#' str(convertIntegers(1.3))
#' str(convertIntegers(c(1.0, 2.0)))
#' str(convertIntegers("foo"))
convertIntegers = function(x) {
  if (is.integer(x))
    return(x)
  if ((is.atomic(x) && all(is.na(x))) ||
      (is.numeric(x) && isTRUE(all.equal(x, as.integer(x)))))
    return(as.integer(x))
  return(x)
}
