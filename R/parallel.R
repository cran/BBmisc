#' Parallelization setup for parallelMap.
#'
#' Defines the underlying parallelization mode (currently multicore or snowfall) for
#' \code{\link{parallelMap}} and allows to set a \dQuote{level} of parallelization. 
#' Only calls to \code{\link{parallelMap}} with a matching level are parallelized. 
#'
#' For snowfall \code{\link[snowfall]{sfStop}}, \code{\link[snowfall]{sfSetMaxCPUs}}, \code{\link[snowfall]{sfInit}}, \code{\link[snowfall]{sfClusterSetupRNG}} 
#' are called in this order.
#'
#' @param mode [\code{character(1)}]\cr
#'   Which parallel mode should be used:
#'   \dQuote{local}, \dQuote{multicore}, \dQuote{snowfall}.
#'   Default is \dQuote{local} without parallel execution.
#' @param cpus [\code{integer(1)}]\cr
#'   Number of used cpus.
#'   Default is \code{\link[Rmpi]{mpi.universe.size}} for snowfall/MPI and 1 otherwise.
#' @param ... [any]\cr
#'    Optional parameters, only passed to \code{\link[snowfall]{sfInit}} currently.
#' @param level [\code{character(1)}]\cr
#'   You can set this so only calls to \code{\link{parallelMap}} are parallelized
#'   that have the same level specified.
#'   Default is \code{NA} which means all calls are parallelized.
#' @param log [\code{character(1)}]\cr
#'   Path to an existing directory where a log files for each job is stored via
#'   \code{\link{sink}}. Note that all nodes must have write access to exactly this path.
#'   Files are named "<iteration_number>.log".
#'   \code{NULL} means no logging and this is the default. 
#' @return Nothing.
#' @export
parallelStart = function(mode="local", cpus, ..., level=as.character(NA), log=NULL) {
  checkArg(mode, choices=c("local", "snowfall", "multicore"))
   
  if (missing(cpus)) {
    if (mode == "multicore")
      cpus = multicore:::detectCores()
    else if(mode=="snowfall" && type=="MPI")
      cpus = Rmpi::mpi.universe.size()
    else 
      cpus = 1L    
  } else {
    cpus = convertInteger(cpus)
    checkArg(cpus, "integer", len=1, na.ok=FALSE)
    if (cpus != 1L && mode == "local")
      stopf("Setting %i cpus makes no sense for local mode!", cpus)
  }
  checkArg(level, "character", len=1, na.ok=TRUE)
  if (!is.null(log)) {
    checkArg(log, "character", len=1, na.ok=FALSE)
    if (!file.exists(log)) 
      stopf("Logging dir 'log' does not exists: %s", log)
    if (!isDirectory(log)) 
      stopf("Logging dir 'log' is not a directory: %s", log)
    if (mode=="local")  
      stop("Logging not supported for local mode!")
  }
  
  type = coalesce(..., "SOCK")
  packs = if (mode == "multicore")
    "multicore"
  else if (mode == "snowfall")
    if (type == "MPI")
      c("snowfall", "Rmpi")
    else
      "snowfall"
  else
    character(0)
  requirePackages(packs, "setupParallel")
  if (mode == "snowfall") {
    sfStop()
    sfSetMaxCPUs(cpus)
    sfInit(parallel=TRUE, cpus=cpus, ...)
    sfClusterSetupRNG()
  }
  options(BBmisc.parallel.mode = mode)
  options(BBmisc.parallel.cpus = cpus)
  options(BBmisc.parallel.level = level)
  options(BBmisc.parallel.log = log)
  invisible(NULL)
}

#' Stops parallelization.
#'
#' For snowfall \code{\link[snowfall]{sfStop}} is called.
#'
#' @return Nothing.
#' @export
parallelStop = function() {
  mode = getOption("BBmisc.parallel.mode")
  if (mode == "snowfall")
    sfStop()
}

# FIXME: do parallel export efficiently
#parallelExport = function(...) {
#  mode = getOption("BBmisc.parallel.mode")
#	# multicore does not require to export because mem is duplicated after fork (still copy-on-write)
#	if (mode == "snowfall") {
#    args = list(...) 
#    ns = names(args)
#    for (i in seq_along(args)) {
#      name = ns[i]
#      obj = args[[i]]
#      hash = digest(c(digest(name), digest(obj)))
#      if (!exists(hash, envir=.BBmisc.parallel.hashes)) { 
#        assign(hash, TRUE, env=.BBmisc.parallel.hashes)
#       sfClusterCall(assign, name, obj, env=globalenv())
#      }
#    }
#	}
#}


#' Maps a function over lists or vectors in parallel.
#'
#' Use the parallelization mode and the other options set in 
#' \code{\link{parallelStart}}. For multicore \code{\link[multicore]{mclapply}}
#' is used, for snowfall \code{\link[snowfall]{sfClusterApplyLB}}.
#' 
#' Currently some options for more efficient exporting of arguments to
#' slaves for snowfall are missing, will be included in next version.
#'
#' @param fun [\code{function}]\cr
#'   Function to map over \code{...}.
#' @param ... [any]\cr
#'   Arguments to vectorize over (list or vector).
#' @param more.args [\code{list}]\cr
#'   A list of other arguments passed to \code{fun}.
#'   Default is empty list.
#' @param simplify [\code{logical(1)}]\cr
#'   Should the result be simplified? 
#'   See \code{\link{sapply}}.
#'   Default is \code{FALSE}.
#' @param use.names [\code{logical(1)}]\cr
#'   Should result be named by first vector if that is 
#'   of class character or integer?
#'   Default is \code{FALSE}.
#' @param level [\code{character(1)}]\cr
#'   The call is only parallelized if the same level is specified in 
#'   \code{\link{parallelStart}} or this argument is \code{NA}.
#'   Default is \code{NA}.
#' @return Result.
#' @export
#' @examples
#' parallelStart()
#' parallelMap(identity, 1:2)
#' parallelStop()
parallelMap = function(fun, ..., more.args=list(), simplify=FALSE, use.names=FALSE, level=as.character(NA)) {
  mode = getOption("BBmisc.parallel.mode")
  cpus = getOption("BBmisc.parallel.cpus")
  lev = getOption("BBmisc.parallel.level")
  log = getOption("BBmisc.parallel.log")
  if (mode == "local" || (!is.na(lev) && !is.na(level) && level==lev)) {
    res = mapply(fun, ..., MoreArgs=more.args, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  } else {
    #parallelExport(...)    
    iters = seq_along(..1)
    toList = function(...) {
      Map(function(iter, ...) {
        c(list(iter), list(...), more.args)
      }, iters, ...)
    }
    if (mode == "multicore") {
      res = multicore::mclapply(toList(...), FUN=slaveWrapper, .fun=fun, .log=log)
    }  else if (mode == "snowfall") {
      res = sfClusterApplyLB(toList(...), fun=slaveWrapper, .fun=fun, .log=log)
    }
  }

  lapply(seq_along(res), function(i) {
    x = res[[i]]
    if (is.error(x)) {
      stopf("On slave %i: %s", i, as.character(x))
    }
  })
  
  if (use.names && (is.character(..1) || is.integer(..1))) {
    names(res) = ..1
  }
  if (isTRUE(simplify) && length(res) > 0)
    res = simplify2array(res, higher = (simplify == "array"))

  return(res)
}

slaveWrapper = function(.x, .fun, .log=NULL) {
  if (!is.null(.log)) {
    options(warning.length=8170, warn=1)
    fn = file.path(.log, sprintf("%03i.log", .x[[1]]))
    fn = file(fn, open="wt")
    sink(fn)
    sink(fn, type="message")
  }  

  res = try(do.call(.fun, .x[-1]), silent = is.null(log))

  if (!is.null(.log)) {
    # FIXME: should we close the file here?
    print(gc())
    sink(NULL)
  }  
  return(res)
}
