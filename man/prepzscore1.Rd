\name{prepzscore1}
\alias{prepzscore1}
\title{
Apply Z-score1 normalization.
}
\description{
Apply Z-score1 normalization before combining the data sets together. Each data sets is Z-score normalized separately and then, the data sets composing the training sets are combined together. The testing set is Z-score normalized independently and separately from the training set.
}
\usage{
prepzscore1(common.gene, geno.files, surv.data, x, y)
}

\arguments{
  \item{common.gene}{
A vector of character strings containing the name of the genes common to the data sets composing the training set.
}
  \item{geno.files}{
A vector of character strings containing the names of expression files.
}
  \item{surv.data}{
The list of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
  \item{x}{
A vector of indices of the names of the expression files composing the training set.
}
  \item{y}{
Index of the name of expression file used as the testing set.
}
}

\value{
A list of two objects, (i) the matrix of combined normalized gene expression data and (ii) a list of two vectors, the combined survival times and censoring status. 
}

\author{
Haleh Yasrebi
}
\seealso{
\code{\link{znorm}}
}
 \section{Warning }{This function is not called by the user directly.} 
\keyword{Z-score normalization}

