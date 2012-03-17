#' Convert single numeric to integer only if the numeric represents a single integer,
#' e.g. 1 to 1L. 
#' Otherwise the argument is returned unchanged.
#' @title Conversion for single integer.
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
  if (length(x) == 1L && ((is.atomic(x) && is.na(x)) || (is.numeric(x) && x == as.integer(x))))
    as.integer(x)
  else
    x
}

#' Convert numeric vector to integer vector if the numeric vector fully represents 
#' an integer vector,
#' e.g. \code{c(1, 5)} to \code{c(1L, 5L)}. 
#' Otherwise the argument is returned unchanged.
#' @title Conversion for integer vector.
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
  if ((is.atomic(x) && all(is.na(x))) || (is.numeric(x) && all(x == as.integer(x))))
    as.integer(x)
  else
    x
}
