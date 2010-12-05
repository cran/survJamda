\name{calPerformance.meta}
\alias{calPerformance.meta}
\title{
Meta analysis of survival data
}
\description{
Analyze jointly the data set by the inverse normal method (Hedges and Olkin, 1985).
}
\usage{
calPerformance.meta(common.gene, zstat, i, j, geno.files, surv.data, method)
}
\arguments{
  \item{common.gene}{
A vector of character strings containing the names of the genes common to all  data sets.
}
  \item{zstat}{
A list containing the combined Z-scores of the data sets composing the training set.
}
  \item{i}{
A vector of character strings containing the names of the data sets composing the training set.
}
  \item{j}{
A character string specifying the name the data set used as the testing set.
}
  \item{geno.files}{
A vector of character strings containing the names of gene expression files.
}
  \item{surv.data}{
A list of two vectors, survival time and censoring status.In the censoring status vector, 1 = event occurred, 0 = censored.
}
  	\item{method}{
		A character string specifying the feature selection method: "none" for top-100 ranking or one of the adjusting methods specified by the p.adjust function.}
}

\details{
In top-ranking, genes are selected based on univariate Cox P-value ranking using the coxph function in the R survival package. In this feature selection method, the genes are ranked based on their likelihood ratio P-value and the top-100 ranked genes with the smallest P-values are retained as the gene signature.

The p.adjust function in the R stats package is used and all adjusted p-value not greater than 0.05 are retained if \code{method} != "none".
}

\value{
AUC, HR(CI) and p-value.
}
\references{
L. V. Hedges and I. Olkin. Statistical Methods for Meta-Analysis. Academic Press,January 1985. ISBN 0123363802.
}
\author{
Haleh Yasrebi
}
\section{Warning }{This function is not called by the user directly.} 
\keyword{Meta analysis}
