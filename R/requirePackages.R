#TODO: option for installing them? I dont like this!
# only if interactive() is TRUE?

#' Requires packages that were only suggested.
#' If some packages could not be loaded the following exception is thrown:
#' \dQuote{For <why> please install the following packages: <missing packages>}.
#' @title Requires packages that were only suggested.
#' @param packs [\code{character}]\cr
#'   Names of packages. 
#' @param why [\code{character(1)}]\cr
#'   Short string explaining why packages are required.
#' @return [\code{logical}]. Logical vectors describing which packages could be loaded.
#'   Same length as \code{packs}.
#' @export 
requirePackages = function(packs, why) {
  # this should be a bit faster...
  packs.ok = sapply(packs, function(x) paste("package", x, sep = ":") %in% search())
  packs = packs[!packs.ok]
  packs.ok = sapply(packs, function(x) require(x, character.only = TRUE))
  if (length(packs.ok) == 0)
    packs.ok = TRUE
  if(!all(packs.ok)) {
    ps = paste(packs[!packs.ok], collapse=" ")
    stop(paste("For", why, "please install the following packages:", ps))
  }
  return(packs.ok)
}
