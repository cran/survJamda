\name{it.sol}
\alias{it.sol}

\title{
Iterative solution for Empirical Bayesian method. 
}
\description{
Iterative solution for Empirical Bayesian method. 
}
\usage{
it.sol(sdat, g.hat, d.hat, g.bar, t2, a, b, conv = 1e-04)
}

\arguments{
  \item{sdat}{
Standardized matrix of gene expression data.
}
  \item{g.hat}{
Estimated additive batch effect.
}
  \item{d.hat}{
Estimated multiplicative batch effect.
}
  \item{g.bar}{
Mean of \code{g.hat}.
}
  \item{t2}{
Variance of the rows of  \code{g.hat}.
}
  \item{a}{
Value of the function \code{aprior} applied to multiplicative batch effect.
}
  \item{b}{
Value of the function \code{bprior} applied to multiplicative batch effect.
}
  \item{conv}{
Convergence threshold.
}
}

\value{
Matrix of estimated additive and multiplicative batch effects.
}
\references{
W. Johnson E., L. Chen, Rabinovic, and A. Adjusting batch effects in microarray expression data using Empirical Bayes methods. Biostatistics, 8(1):118-127, Jan
ary2007. ISSN 1465-4644. doi: \url{http://dx.doi.org/10.1093/biostatistics/kxj037}.

}
\author{
WE Johnson.
}
\seealso{
\code{\link{ComBat},\link{aprior},\link{bprior}}
}
 \section{Warning }{This function is not called by the user directly.} 
\keyword{ComBat}

