\name{shuffle.samples}
\alias{shuffle.samples}
\title{
Shuffle samples.
}
\description{
To ensure the applicability of Cox regression, the function splits the samples randomly and assigns at least one deceased or relapsed patient to the training and testing sets.
}
\usage{
shuffle.samples(n, censor, train.nb)
}

\arguments{
  \item{n}{
Sample size of the complete data set.
}
  \item{censor}{
Vector of censoring status. 1 = event occurred, 0 = censored.
}
  \item{train.nb}{
Number of samples in the training set.
}
}

\value{
List of two vectors, the indices of the training set and the indices of the testing set.
}

\author{
Haleh Yasrebi
}
\section{Warning }{This function is not called by the user directly.} 

