\name{filter.absent}
\alias{filter.absent}

\title{
Filter absent calls
}
\description{
Filter data based on presence/absence call.
}
\usage{
filter.absent(x, pct)
}

\arguments{
  \item{x}{
Matrix of gene expression data.
}
  \item{pct}{
Number of presence calls.
}
}

\value{
A Boolean variable specifying the presence or absence call.
}
\references{
W. Johnson E., L. Chen, Rabinovic, and A. Adjusting batch effects in microarray
expression data using Empirical Bayes methods. Biostatistics, 8(1):118-127, January2007. ISSN 1465-4644. doi: \url{http://dx.doi.org/10.1093/biostatistics/kxj037}.

}
\author{
WE Johnson

}
\seealso{
\code{\link{ComBat}}
}
 \section{Warning }{This function is not called by the user directly.} 
\keyword{
ComBat
}
