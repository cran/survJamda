\name{postvar}
\alias{postvar}

\title{
Estimated multiplicative batch effect
}
\description{
Estimated multiplicative batch effect
}
\usage{
postvar(sum2, n, a, b)
}

\arguments{
  \item{sum2}{
Batch sample variance.
}
  \item{n}{
A vector containing the sum of the rows of standardized gene expression data.
}
  \item{a}{
Value of \code{aprior} function applied to multiplicative batch effect.
}
  \item{b}{
Value of \code{bprior} function applied to multiplicative batch effect.
}
}

\value{
Estimated multiplicative batch effect
}
\references{
W. Johnson E., L. Chen, Rabinovic, and A. Adjusting batch effects in microarray
expression data using Empirical Bayes methods. Biostatistics, 8(1):118-127, January2007. ISSN 1465-4644. doi: \url{http://dx.doi.org/10.1093/biostatistics/kxj037}.

}
\author{
WE Johnson
}

\seealso{
\code{\link{ComBat},\link{aprior},\link{bprior}}
}

 \section{Warning }{This function is not called by the user directly.} 
\keyword{ComBat}
