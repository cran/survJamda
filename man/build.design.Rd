\name{build.design}
\alias{build.design}

\title{
Initiation to build the design matrix
}
\description{
Initiation to build the design matrix.
}
\usage{
build.design(vec, des = NULL, start = 2)
}

\arguments{
  \item{vec}{
Vector of batches in the sample info matrix.
}
  \item{des}{
Initial value of design matrix
}
  \item{start}{
Starting index of design matrix.
}
}

\value{
Design matrix
}
\references{
W. Johnson E., L. Chen, Rabinovic, and A. Adjusting batch effects in microarray
expression data using Empirical Bayes methods. Biostatistics, 8(1):118-127, January2007. ISSN 1465-4644. 
}

\author{
WE Johnson
}
 \section{Warning }{This function is not called by the user directly.} 

\keyword{ComBat}
