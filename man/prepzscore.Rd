\name{prepzscore}
\alias{prepzscore}
\title{
Z-score normalization.
}
\description{
Take two data sets, apply Z-score normalization to each and combine the two normalized data sets.
}
\usage{
prepzscore(i, j)
}

\arguments{
  \item{i}{
Matrix of gene expression data of the one of the two single data sets.
}
  \item{j}{
Matrix of gene expression data of the other single data set.
}
}

\value{
Matrix of Z-score normalized merged data set.
}

\author{
Haleh Yasrebi
}

\examples{
require(survJamda.data)

data(gse4335)
data(gse1992)

common.gene = intersect(colnames(gse4335),colnames(gse1992))
#m = prepzscore(gse4335[,common.gene],gse1992[,common.gene])

## The function is currently defined as
function (i, j)
{
        i = scale(t(scale(t(i))))
        j = scale(t(scale(t(j))))
        mat = rbind(i,j)
        return(mat)
}
}
\keyword{Z-score normalization}
