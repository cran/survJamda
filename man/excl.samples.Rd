\name{excl.samples}
\alias{excl.samples}

\title{
Exclude samples
}
\description{
Time-dependent ROC curves could be plotted based on different survival time points at which an event has occurred. To this end, censored patients should be excluded so that the plot could be based on the time points at which an event has occurred.
}
\usage{
excl.samples(test.ind, surv, censor)
}

\arguments{
  \item{test.ind}{
Vector of testing set index.
}
  \item{surv}{
Vector of survival time.
}
  \item{censor}{
Vector of censoring status.
}
}

\value{
Index of survival time points of the patients in the testing set who have experienced an event.
}

\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.} 
