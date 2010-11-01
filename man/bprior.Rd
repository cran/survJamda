\name{bprior}
\alias{bprior}

\title{
Calculate empirical hyper-prior values of Bayesian model
}
\description{
Calculate empirical hyper-prior values of Bayesian model
}
\usage{
bprior(gamma.hat)
}

\arguments{
  \item{gamma.hat}{
Estimate of additive batch effect
}
}

\value{
Empirical hyper-prior values of Bayesian model.
}
\references{
W. Johnson E., L. Chen, Rabinovic, and A. Adjusting batch effects in microarray
expression data using Empirical Bayes methods. Biostatistics, 8(1):118-127, January2007. ISSN 1465-4644. doi: \url{http://dx.doi.org/10.1093/biostatistics/kxj037}.

}
\author{
WE Johnson
}
\seealso{
\code{\link{ComBat},\link{aprior}}
}

 \section{Warning}{This function is not called by the user directly.} 

\keyword{ComBat}

