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
  used to display progress in the bar.}
}
\value{
  [\code{function(value, msg="")}]. A progress bar
  function.  Call it during a loop to change the display
  the progress bar.  The message can be set to change the
  label in front of the bar.
}
\description{
  Create a progress bar function that displays estimated
  time till completion.
}

