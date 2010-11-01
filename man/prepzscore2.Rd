\name{prepzscore2}
\alias{prepzscore2}

\title{
Apply Z-score2 normalization.
}
\description{
Apply Z-score2 normalization. First, combine the data sets composing the training set and then, apply Z-score normalization to the merged data set. 
}
\usage{
prepzscore2(common.gene, geno.files, surv.data, x, y)
}
\arguments{
  \item{common.gene}{
Vector of character strings containing the names of the genes common to the all data sets.
}
  \item{geno.files}{
A vector of character strings containing the names of gene expression files.
}
  \item{surv.data}{
The list of two vectors, survival time and censoring status related to the training set.
}
  \item{x}{
A vector of indices of the names of data files composing the training set.
}
  \item{y}{
Index of the name of data file used as the testing set.
}
}

\value{
A list of two objects related to the training set, (i) the matrix of Z-score normalized merged gene expression data and (ii) a list of two vectors, the combined survival time and censoring status. 
}

\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.} 

\keyword{Z-score normalization}
