\name{prepcombat}
\alias{prepcombat}
\title{
Combination of data sets prior to the application of ComBat.
}
\description{
Combine the gene expression data, survival time and censoring status of at least two data sets prior to the application of ComBat.
}
\usage{
prepcombat(common.gene, geno.files, surv.data, batchID, x, y)
}

\arguments{
  \item{common.gene}{
Vector of character strings specifying the names of the genes common to all single data sets.
}
  \item{geno.files}{
Vector of character strings specifying the names of gene expression files.
}
  \item{surv.data}{
The list of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
\item{batchID}{
For a given data set, the batch id can be an integer or the name of the data set. The batch id must be the same for all samples or arrays of a data set. 
}

  \item{x}{
Vector of character strings specifying the names of gene expression files composing the training set.
}
  \item{y}{
A vector of character string specifying the name of gene expression file used as the testing set.
}
}

\value{
A list of two objects, (i) ComBat-adjusted gene expression data and (ii) the list of two vectors, the merged survival time and censoring status.
}

\author{
Haleh Yasrebi
}

\section{Warning }{This function is not called by the user directly.} 
\keyword{ComBat}

