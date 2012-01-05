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
#    Default is \sQuote{+}.
#' @return [\code{function(value, msg="")}]. A progress bar function. 
#'   Call it during a loop to change the display the progress bar. 
#'   The message can be set to change the label in front of the bar.
#' @export
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
  
  function(value, msg=label) {
    checkArg(value, "numeric", len=1, na.ok=FALSE, lower=min, upper=max)
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
        msg, paste(bar, collapse=""), round(rate*100),
        rest.time["hours"], rest.time["minutes"], rest.time["seconds"])
      if (value == max) {
        cat("\n")
        did.final.newline <<- TRUE
      }
      flush.console()
    }
  }
}
