#' Create a progress bar function that displays estimated time till
#' completion.
#' @title Create a progress bar.
#' @param min [\code{numeric(1)}]\cr
#'   Minimum value, default is 0.
#' @param max [\code{numeric(1)}]\cr
#'   Maximum value, default is 100.
#' @param label [\code{character(1)}]\cr
#'   Label shown in front of the progress bar. 
#'   Note, that if you later set \code{msg} in the progress bar function, 
#'   the message will be left-padded to the length of this label, therefore
#'   it should be at least as long as the longest message you want to display.
#'   Default is \dQuote{}.
#' @param char [\code{character(1)}]\cr
#'   A single character used to display progress in the bar. 
#'   Default is \sQuote{+}.
#' @return [\code{function(value, inc, msg="")}]. A progress bar function. 
#'   Call it during a loop to change the display the progress bar. 
#'   You can either set the current \code{value} or increment the current value
#'   by using \code{inc}. Note that you are not allowed to decrease the value by setting
#'   \code{value}. If you call this function without setting any of the former two arguments,
#'   the bar is simply redrawn with the current value.
#'   The message can be set to change the label in front of the bar.
#' @export
#' @examples
#' bar <- makeProgressBar(max=5, label="test-bar")
#' for (i in 0:5) {
#'   bar(i)
#'   Sys.sleep(0.5)
#' }
#' bar <- makeProgressBar(max=5, label="test-bar")
#' for (i in 1:5) {
#'   bar(inc=1)
#'   Sys.sleep(0.5)
#' }
makeProgressBar = function(min=0, max=100, label="", char="+") {
  checkArg(min, "numeric", len=1, na.ok=FALSE)                          
  checkArg(max, "numeric", len=1, na.ok=FALSE)                          
  checkArg(label, "character", len=1, na.ok=FALSE)                          
  ## label |................................| xxx% (hh:mm:ss)
  console.width = options()$width
  label.width = nchar(label)
  bar.width = console.width - label.width - 21
  bar = rep(" ", bar.width)

  start.time = as.integer(Sys.time())
  delta = max - min
  kill.line = "\r"
  did.final.newline = FALSE  
  cur.value = min

  function(value, inc, msg=label) {
    if (!missing(value) && !missing(inc))
      stop("You must not set value and inc!")
    else if (!missing(value))
      checkArg(value, "numeric", len=1, na.ok=FALSE, lower=max(min,cur.value), upper=max)
    else if (!missing(inc)) {
      checkArg(inc, "numeric", len=1, na.ok=FALSE, lower=0, upper=max-cur.value)
      value = cur.value + inc
    } else {
      value = cur.value
    }
    
    if (!did.final.newline)  {
      rate = (value - min) / delta 
      bin = round(rate * bar.width)
      bar[seq(bin)] <<- char
      delta.time = as.integer(Sys.time()) - start.time
      if (value == min)
        rest.time = 0
      else
        rest.time = (max - value) * (delta.time / (value - min))
      rest.time = splitTime(rest.time, "hours")
      # as a precaution, so we can _always_ print in the progress bar
      if (rest.time["hours"] > 99)
        rest.time[] = 99
      cat(kill.line)
      msg = str_pad(msg, label.width)
      catf("%s |%s| %3i%% (%02i:%02i:%02i)", newline=FALSE,
        msg, collapse(bar, sep=""), round(rate*100),
        rest.time["hours"], rest.time["minutes"], rest.time["seconds"])
      if (value == max) {
        cat("\n")
        did.final.newline <<- TRUE
      }
      flush.console()
    }
    cur.value <<- value
  }
}
