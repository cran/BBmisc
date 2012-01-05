#' Is one / are several files a directory?
#' If a file does not exist, \code{FALSE} is returned.
#' @title Is one / are several files a directory?
#' @param ... [\code{character(1)}]\cr
#'   File names, all strings.
#' @return [\code{logical}]. 
#' @export
isDirectory = function(...) {
  x = file.info(...)$isdir
  !is.na(x) & x
}


#' Is one / are several directories empty?
#' If file does not exist or is not a directory, \code{FALSE} is returned.
#' @title  Is one / are several directories empty?
#' @param ... [\code{character(1)}]\cr
#'   Directory names, all strings.
#' @return [\code{logical}]. 
#' @export
isEmptyDirectory = function(...) {
  vapply(list(...), FUN.VALUE=logical(1), FUN=function(x) {
    isDirectory(x) && length(list.files(x, all.files=TRUE, include.dirs=TRUE)) == 2
  })
}
