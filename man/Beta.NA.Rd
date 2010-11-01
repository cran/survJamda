\name{Beta.NA}
\alias{Beta.NA}

\title{
Fit the L/S model in the presence of missing data values
}
\description{
Fit the L/S model in the presence of missing data values
}
\usage{
Beta.NA(y, X)
}

\arguments{
  \item{y}{
Product of design matrix and matrix of gene expression data.
}
  \item{X}{
Matrix of gene expression data.
}
}

\value{
Vector of Regression coefficients in L/S model fitting.
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
