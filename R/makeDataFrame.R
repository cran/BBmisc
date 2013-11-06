#' Initialize data.frame in a convenient way.
#'
#' @param nrow [\code{integer(1)}]\cr
#'   Nubmer of rows.
#' @param ncol [\code{integer(1)}]\cr
#'   Number of columns.
#' @param col.types [\code{character(ncol)} | \code{character(1)}]\cr
#'   Data types of columns.
#'   If you only pass one type, it will be replicated.
#' @param init [any]\cr
#'   Scalar object to initialize all elements of the data.frame.
#'   You do not need to specify \code{col.types} if you pass this.
#' @param row.names [\code{character(nrow)} | \code{NULL}]\cr
#'   Row names.
#'   Default is \code{NULL}.
#' @param col.names [\code{character(ncol)}]\cr
#'   Column names.
#'   Default is \dQuote{V1}, \dQuote{V2}, and so on.
#' @export
#' @examples
#' print(makeDataFrame(3, 2, init=7))
#' print(makeDataFrame(3, 2, "logical"))
#' print(makeDataFrame(3, 2, c("logical", "numeric")))
makeDataFrame = function(nrow, ncol, col.types, init, row.names=NULL, col.names=sprintf("V%i", seq_len(ncol))) {
  nrow = convertInteger(nrow)
  checkArg(nrow, "integer", len=1L, na.ok=FALSE, lower=0L)
  ncol = convertInteger(ncol)
  checkArg(ncol, "integer", len=1L, na.ok=FALSE, lower=0L)
  if (!missing(col.types)) {
    checkArg(col.types, "character", min.len=1L, na.ok=FALSE)
  }
  if (!missing(init)) {
    if(!isScalarValue(init))
      stop("'init' must be a scalar value!")
    if (!missing(col.types)) {
      if (length(col.types) > 1L)
        stop("If 'init' is given, length of col.types must be 1!")
      if (identical(class(init)[1L], "col.types"))
        stop("Class of 'init' must match given column type!")
    }
  }
  if (!missing(col.types) && length(col.types) == 1L)
    col.types = rep.int(col.types, ncol)
  if (!is.null(row.names))
    checkArg(row.names, "character", len=nrow, na.ok=TRUE)
  checkArg(col.names, "character", len=ncol, na.ok=TRUE)

  if (nrow == 0L && ncol == 0L)
    df = data.frame()
  else if (ncol == 0L)
    df = data.frame(matrix(nrow=nrow, ncol=0))
  else if (missing(init))
    df = lapply(col.types, vector, length=nrow)
  else
    df = replicate(ncol, rep.int(init, nrow), simplify=FALSE)
  df = as.data.frame(df, stringsAsFactors=FALSE)
  rownames(df) = row.names
  colnames(df) = col.names
  return(df)
}