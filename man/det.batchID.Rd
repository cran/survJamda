\name{det.batchID}
\alias{det.batchID}
\title{
Determine the batch ID of data sets.
}
\description{
Determine the batch ID of data sets for ComBat.
}
\usage{
det.batchID()
}

\value{
A vector of integers specifying the batch ID of data sets. The integers from 1 to the number specifying the length of \code{geno.files} are set as the batch ID of the data sets named in \code{geno.files} as follows: 1 to the first expression file name in \code{geno.files}, 2 to the second expression file name in \code{geno.files}, ... and the integer specifying the length of \code{geno.files} to the last expression file in \code{geno.files}, respectively.
}

\author{
Haleh Yasrebi
}

\keyword{ComBat}

