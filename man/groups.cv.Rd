\name{groups.cv}
\alias{groups.cv}
\title{
Split a data set for cross-validation
}
\description{
Define the folds of cross-validation.
}
\usage{
groups.cv(n, ngroup, censor)
}

\arguments{
  \item{n}{
Sample size of a data set.
}
  \item{ngroup}{
Number of folds of cross-validation.
}
  \item{censor}{
Vector of censoring status.
}
}
\details{
The function avoids allocating only censored patients to the testing set. At least one patient having experienced an event is needed for the applicability of the Cox proportional hazard model.
}
\value{
The folds of cross-validation.
}

\author{
Haleh Yasrebi
}

\section{Warning }{This function is not called by the user directly.} 

