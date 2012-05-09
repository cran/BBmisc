#' Split seconds into handy chunks of time.
#' 
#' Note that a year is simply defined as exactly 365 days.
#'
#' @param seconds [\code{numeric(1)}]\cr
#'   Number of seconds. If not an integer, it is rounded down.
#' @param unit [\code{character(1)}]\cr
#'   Largest unit to split seconds into. Must be one of the ones
#'   listed in the function signature.
#' @return [\code{numeric(5)}]. A named vector containing the
#' \dQuote{years}, \dQuote{days}, \dQuote{hours}, \dQuote{minutes}
#' and \dQuote{seconds}. Units larger than the given \code{unit} are
#' \code{NA}.
#' @export
#' @examples
#' splitTime(1000)
splitTime = function(seconds,
                      unit=c("years", "days", "hours", "minutes", "seconds")) {
  unit = match.arg(unit)
  res = c(years=NA, days=NA, hours=NA, minutes=NA, seconds=NA)
  divider = c(31536000, 86400, 3600, 60, 1)
  start = which(names(res) == unit)
  for (i in start:length(divider)) {
    res[i] = seconds %/% divider[i]
    seconds = seconds - res[i] * divider[i]
  }
  res
}
