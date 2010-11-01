\name{writeGeno}
\alias{writeGeno}
\title{
Reformat gene expression data for ComBat.
}
\description{
Reformat gene expression data for ComBat.
}
\usage{
writeGeno(x, fileName)
}
\arguments{
  \item{x}{
Matrix of gene expression data to be adjusted.
}
  \item{fileName}{
A character string specifying the name of the file in which the adjusted data should be saved.
}
}

\value{
None. 
}

\author{
Haleh Yasrebi
}

\seealso{
 \code{\link{writeSamples},\link{ComBat}}
}

\section{Warning }{This function is not called by the user directly.} 
\keyword{ComBat}
