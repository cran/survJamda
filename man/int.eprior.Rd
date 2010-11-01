\name{int.eprior}
\alias{int.eprior}

\title{
Integration function to find nonparametric adjustments
}
\description{
Monte Carlo integration function to find the nonparametric adjustments
}
\usage{
int.eprior(sdat, g.hat, d.hat)
}

\arguments{
  \item{sdat}{
Matrix of standardized gene expression data.
}
  \item{g.hat}{
Estimated additive batch effect.
}
  \item{d.hat}{
Estimated multiplicative batch effect.
}
}

\value{
Matrix with two columns containing the estimated additive and multiplicative batch effects.
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

\keyword{ComBat}

