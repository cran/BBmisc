#' Evaluates an expression and suppresses all output except for errors, 
#' meaning: prints, messages, warnings and package startup messages.
#' @title Suppresses all output except for errors.
#' @param expr [valid R expression]\cr
#'   Expression. 
#' @return Return value of expression invisibly. 
#' @export 
suppressAll = function(expr) {
  capture.output({
    z = suppressWarnings(
      suppressMessages(
        suppressPackageStartupMessages(force(expr))                  
      )
    )
  })
  invisible(z)
}
