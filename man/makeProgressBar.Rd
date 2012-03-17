\name{makeProgressBar}
\alias{makeProgressBar}
\title{Create a progress bar.}
\usage{
  makeProgressBar(min = 0, max = 100, label = "",
    char = "+")
}
\arguments{
  \item{min}{[\code{numeric(1)}]\cr Minimum value, default
  is 0.}

  \item{max}{[\code{numeric(1)}]\cr Maximum value, default
  is 100.}

  \item{label}{[\code{character(1)}]\cr Label shown in
  front of the progress bar.  Note, that if you later set
  \code{msg} in the progress bar function, the message will
  be left-padded to the length of this label, therefore it
  should be at least as long as the longest message you
  want to display.  Default is \dQuote{}.}

  \item{char}{[\code{character(1)}]\cr A single character
  used to display progress in the bar.  Default is
  \sQuote{+}.}
}
\value{
  [\code{function(value, inc, msg="")}]. A progress bar
  function.  Call it during a loop to change the display
  the progress bar.  You can either set the current
  \code{value} or increment the current value by using
  \code{inc}. Note that you are not allowed to decrease the
  value by setting \code{value}. If you call this function
  without setting any of the former two arguments, the bar
  is simply redrawn with the current value.  The message
  can be set to change the label in front of the bar.
}
\description{
  Create a progress bar function that displays estimated
  time till completion.
}
\examples{
bar <- makeProgressBar(max=5, label="test-bar")
for (i in 0:5) {
  bar(i)
  Sys.sleep(0.5)
}
bar <- makeProgressBar(max=5, label="test-bar")
for (i in 1:5) {
  bar(inc=1)
  Sys.sleep(0.5)
}
}

