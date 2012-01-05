#' Prints object to a string / character vector.
#' @title Prints object to a string / character vector.
#' @param x [any]\cr
#'   Object to print
#' @param collapse [\code{character(1)}]\cr
#'   Used to collapse multiple lines.
#'   \code{NULL} means no collapsing, vector is returned.
#'   Default is \dQuote{\\n}.
#' @return [\code{character}].
#' @export
printToChar = function(x, collapse="\n") {
  rval = NULL
  con = textConnection("rval", "w", local = TRUE)
  sink(con)
  on.exit({
    sink()
    close(con)
  })
  print(x)
  # on.exit()
  # sink()
  # close(con)
  if (!is.null(collapse))
    paste(rval, collapse=collapse)
  else
    rval
}
