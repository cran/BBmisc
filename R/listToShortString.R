#' Converts list to short string describing contents.
#' Looks like this \dQuote{a=1,<unamed>=2,b=<data.frame>}.
#' Vectors are displayed till a certain length.
#' @title Converts list to short string describing contents.
#' @param xs [\code{list}]\cr
#'   The list. 
#' @return [\code{character(1)}].
#' @export
#' @examples
#' listToShortString(list(a=1, b=NULL, "foo", c=1:10))
listToShortString = function(xs) {
  ns = names(xs)
  if (is.null(ns)) 
    ns = rep("<unnamed>", length(xs))
  i = which(is.na(ns) | ns == "")  
  ns[i] = "<unnamed>"
  if (length(xs) == 0)
    return("")
  ss = lapply(xs, function(x) {
    if (is.atomic(x) && length(x) <= 1)
      x
    else if (is.atomic(x))
      capture.output(str(x, nchar.max=10, give.attr=FALSE, give.head=FALSE, indent.str=""))
    else
      paste("<", class(x)[1], ">", sep="")
  })
  paste(paste(ns, "=", ss, sep=""), collapse=", ")
}
