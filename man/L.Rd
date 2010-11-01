\name{L}
\alias{L}

\title{
Likelihood function.
}
\description{
Likelihood function.
}
\usage{
L(x, g.hat, d.hat)
}

\arguments{
  \item{x}{
Matrix of gene expression data.
}
  \item{g.hat}{
Estimated additive batch effect.	
}
  \item{d.hat}{
Estimated multiplicative batch effect.	
}
}

\value{
Likelihood estimate.
}
\references{
W. Johnson E., L. Chen, Rabinovic, and A. Adjusting batch effects in microarray
expression data using Empirical Bayes methods. Biostatistics, 8(1):118-127, January2007. ISSN 1465-4644. doi: \url{http://dx.doi.org/10.1093/biostatistics/kxj037}.
}
\author{
WE Johnson
}

 \section{Warning }{This function is not called by the user directly.} 
