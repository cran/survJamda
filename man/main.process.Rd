\name{main.process}
\alias{main.process}
\title{
main.process
}
\description{
Plot time-dependent ROC curves based on different time points.
}
\usage{
main.process(common.gene, geno.files, surv.data, batchID,method = "none")
}
\arguments{
  \item{common.gene}{
Vector of character strings containing the names of the genes common to all
data sets.
}
  \item{geno.files}{
Vector of character strings containing the names of expression files.
}
  \item{surv.data}{
The list of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
\item{batchID}{
For a given data set, the batch id can be an integer or just the name of the data set. The batch id must be the same for all samples or arrays of a data set. 
}
\item{method}{
		A character string specifying the feature selection method: "none" for top-100 ranking or one of the adjusting methods specified by the p.adjust function}
}
\details{
In top-ranking, genes are selected based on univariate Cox P-value ranking using the coxph function in the R survival package. In this feature selection method, the genes are ranked based on their likelihood ratio P-value and the top-100 ranked genes with the smallest P-values are retained as the gene signature.

The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".
}

\value{
 ROC curves plot and AUC values on the plot.
}
\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.}

