\name{znorm}
\alias{znorm}
\title{
Matrix Z-score normalization.
}
\description{
Z-score normalization of a matrix
}
\usage{
znorm(m)
}

\arguments{
  \item{m}{
Matrix of gene expression data.
}
}

\value{
Z-score normalized matrix of gene expression data \code{m}.
}

\author{
Haleh Yasrebi
}
\references{
Larsen, R. J. and Marx, M. L. (2000). An Introduction to Mathematical Statistics and Its Applications (3rd Edition), Prentice Hall.ISBN 0139223037.
}

\examples{
require(survJamda.data)

data(gse4335)
data(gse3143)

common.gene = intersect(colnames(gse3143), colnames(gse4335))
m = znorm(rbind(gse3143[,common.gene],gse4335[,common.gene]))

## The function is currently defined as
function(m)
{
        m = scale(t(scale(t(m))))
        return(m)
}
}
\keyword{Z-score normalization}

