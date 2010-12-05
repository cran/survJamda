\name{calPerformance.merge.indep}
\alias{calPerformance.merge.indep}
\title{
Assess performance derived from the merged data set by independent validation}
\description{
 Identify a gene signature from the merged data set and reduce of the gene set in the training and testing sets accordingly. The performance of the gene signature is performed by independent validation.
}
\usage{
calPerformance.merge.indep(lst, train.ind, test.ind, method,gn.nb)
}
\arguments{
  \item{lst}{
 List of two objects, the gene expression data matrix and a list 
of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
  \item{train.ind}{
     Index of training set.
}
  \item{test.ind}{
     Index of testing set.
}
  
\item{method}{
	A character string specifying the feature selection method: "none" for top-ranking or one of the adjusting methods specified by the p.adjust function.
}
\item{gn.nb}{
Number of genes to select for gene signature when \code{method}="none".
}
}


\details{
In top-ranking, genes are selected based on univariate Cox P-value ranking using the coxph function in the R survival package. In this feature selection method, the genes are ranked based on their likelihood ratio P-value and the top-100 ranked genes with the smallest P-values are retained as the gene signature.

The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".
} 
\references{
Yasrebi H,  Sperisen P,  Praz V,  Bucher P, 2009 Can Survival Prediction Be Improved By Merging Gene Expression Data Sets?. PLoS ONE 4(10): e7431. doi:10.1371/journal.pone.0007431.
}

\value{
AUC, HR(CI) and p-value.
}

\author{
Haleh Yasrebi
}
\section{Warning }{This function is not called by the user directly.} 
