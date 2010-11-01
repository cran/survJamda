\name{postmean}
\alias{postmean}

\title{
Estimated additive batch effect
}
\description{
Estimated additive batch effect
}
\usage{
postmean(g.hat, g.bar, n, d.star, t2)
}

\arguments{
  \item{g.hat}{
Theoretical estimated additive batch effect.
}
  \item{g.bar}{
Mean of \code{g.hat}
}
  \item{n}{
A vector containing the sum of the rows of standardized gene expression data.
}
  \item{d.star}{
Multiplicative batch effect.
}
  \item{t2}{
Variance of the rows of  \code{g.hat}.
}
}

\value{
Empirical estimated additive batch effect.
}
\references{
W. Johnson E., L. Chen, Rabinovic, and A. Adjusting batch effects in microarray
expression data using Empirical Bayes methods. Biostatistics, 8(1):118-127, January2007. ISSN 1465-4644. doi: \url{http://dx.doi.org/10.1093/biostatistics/kxj037}.

}
\author{
WE Johnson
}

\seealso{
\code{\link{ComBat}, \link{it.sol}}
}

\section{Warning }{This function is not called by the user directly.} 
\keyword{ComBat}

