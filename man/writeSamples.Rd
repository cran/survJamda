\name{writeSamples}
\alias{writeSamples}
\title{
Write batch samples for ComBat.
}
\description{
Create a file for the batch IDs of the data sets.
}
\usage{
writeSamples(x, batchID, fileName)
}
\arguments{
  \item{x}{
Matrix of gene expression data.
}
\item{batchID}{
A vector containing the batch IDs of the data set \code{x}. The batch ID of the data sets composing the matrix \code{x} should be in the same order of the component data sets. 
}
  \item{fileName}{
A character string specifying the name of the file to be created.
}
}
\details{
This function writes two columns in a file: Array.name and Batch. The Array.name column contains the array or sample ID which are the row names of the matrix \code{x}. The batch id in the second column can be an integer or the name of the data set. The batch id must be the same for all samples or arrays of a data set.
}
\value{
None.
}

\author{
Haleh Yasrebi
}
\seealso{
 \code{\link{writeGeno},\link{ComBat}}
}

\section{Warning }{This function is not called by the user directly.} 
\keyword{ComBat}

