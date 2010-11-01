\name{prepcombat.single.indep}
\alias{prepcombat.single.indep}
\title{
Pair-wise combination of single data sets prior to the application of ComBat and independent validation.
}
\description{
Combine in a pair-wise manner the gene expression values, survival time and censoring status of two single data sets prior to the application of ComBat.
}
\usage{
prepcombat.single.indep(ds1, ds2,i, j, batchID)
}

\arguments{
 \item{ds1}{
Matrix of gene expression data of one of the single data sets.
}
 \item{ds2}{
Matrix of gene expression data of the other single data sets.
}
\item{i}{
An integer or character string specifying the batch ID of the data set \code{ds1}
}
\item{j}{
An integer or character string specifying the batch ID of the data set \code{ds2}
}
\item{batchID}{
The batch id can be an integer for a given data set or the name of a data set. The batch id must be the same for all samples or arrays of a data set. 
}
}

\value{
ComBat-adjusted merged gene expression data.
}

\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.} 
\keyword{ComBat}
