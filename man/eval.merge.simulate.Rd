\name{eval.merge.simulate}
\alias{eval.merge.simulate}

\title{
Performance evaluation by merging two simulated independent data sets
}
\description{
Simulate two data sets, merge them and evaluate the performance of the gene signature derived from the merged data set in 10 iterations of 10-fold cross-validation. The data sets are combined into one set, split into the training and testing sets which are then normalized by Z-score normalization.
}
\usage{
eval.merge.simulate(d1, d2, tot.genes, gene.nb, zscore)
}
\arguments{
  \item{d1}{
Matrix of gene expression data of the first simulated data sets.
}
  \item{d2}{
Matrix of gene expression data of the second simulated data set.
}
  \item{tot.genes}{
Number of total genes.
}
  \item{gene.nb}{
Number of true survival genes to identify. 
}
  \item{zscore}{
An integer (1 or 0) specifying whether to apply Z-score normalization or not.
}
}

\value{
None.
}

\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.} 

\seealso{
\code{\link{proc.simulate}}
}
