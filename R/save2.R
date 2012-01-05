#' A simple wrapper for \code{save}. Understands key = value syntax to save 
#' objects using arbitrary variable names. All options of \code{\link[base]{save}}, 
#' except \code{list} and \code{envir}, are available and passed to 
#' \code{\link[base]{save}}.
#' @title Save multiple objects to a file.
#' @param ... [\code{any}]\cr
#'   Will be converted to an environment and then passed to \code{\link[base]{save}}.
#' @param file 
#'   See help of \code{\link[base]{save}}.
#' @param ascii
#'   See help of \code{\link[base]{save}}.
#' @param version
#'   See help of \code{\link[base]{save}}.
#' @param compress
#'   See help of \code{\link[base]{save}}.
#' @param compression_level
#'   See help of \code{\link[base]{save}}.
#' @param eval.promises
#'   See help of \code{\link[base]{save}}.
#' @param precheck
#'   See help of \code{\link[base]{save}}.
#' @return See help of \code{\link[base]{save}}.
#' @export
save2 = function(..., file, ascii = FALSE, version = NULL, compress = !ascii, 
                 compression_level, eval.promises = TRUE, precheck = TRUE) {
  ddd = list(...)
  names.ddd = names(ddd)
  missing.ddd = (names.ddd == "")
  if(any(missing.ddd)) {
    names.sub = as.character(substitute(list(...)))[-1L]
    names.ddd[missing.ddd] = names.sub[missing.ddd]
    names(ddd) = names.ddd
  }

  ee = tryCatch(as.environment(ddd), 
                error = function(e) { stop("Unable to convert to environment (", e, ")") })
  save(list = names.ddd, envir = ee, file = file, ascii = ascii, version = version, compress = compress,
       compression_level = compression_level, eval.promises = eval.promises, precheck = precheck)
}
