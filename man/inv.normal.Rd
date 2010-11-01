\name{inv.normal}
\alias{inv.normal}
\title{
Apply the inverse normal method.
}
\description{
Apply the inverse normal method (Hedges and Olkin, 1985). For each data set, combine the Z-scores or Z-tests and divide them by the square root of the sample size of the data set.
}
\usage{
inv.normal(i, zstat)
}
\arguments{
  \item{i}{
Vector of integer variables containing the indices of data sets.
}
  \item{zstat}{
List of numeric vectors representing Z-tests or Z-scores.
}
}

\value{
Vector of combined Z-tests.
}
\references{
L. V. Hedges and I. Olkin. Statistical Methods for Meta-Analysis. Academic Press,January 1985. ISBN 0123363802.
}
\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.} 
