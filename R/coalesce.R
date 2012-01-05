#' Returns first non-missing, non-null argument, otherwise
#' \code{NULL}.
#'
#' @title Returns first non-missing, non-null argument.
#' @param ... [any]\cr
#'   Arguments.    
#' @return [any]. 
#' @export
coalesce = function(...) {
  dots = match.call(expand.dots=FALSE)$...
  for (arg in dots) {
    is_missing = if (is.symbol(arg)) {
      eval(substitute(missing(symbol), list(symbol=arg)),
           env=parent.frame())
    } else {
      FALSE
    }
    if (!is_missing) {
      value = tryCatch(eval(arg, env=parent.frame()),
                       error=function(...) NULL)
      if (!is.null(value)) {
        return(value)
      }
    }
  }
  NULL
}
