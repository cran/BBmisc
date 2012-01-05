#' Check that a list contains only elements of a required type. 
#' Throws exception if check is not passed.
#' Note that argument is evaluated when checked.
#' @title Check that a list contains only elements of a required type.
#' @param xs [\code{list}]\cr
#'   Argument.
#' @param cl [\code{character(1)}]\cr
#'   Class that elements must have. Checked with \code{is}.
#' @return Nothing.
#' @export
checkListElementClass = function(xs, cl) {
  s = deparse(substitute(xs))
  sapply(seq_along(xs), function(i) {
    x = xs[[i]] 
    if(!(is(x, cl)))
      stop("List ", s, " has element of wrong type ", class(x)[1], " at position ", i, ". Should be: ", cl)
  })  
}
