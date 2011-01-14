\name{featureselection}
\alias{featureselection}
\title{
Apply a feature selection
}
\description{
Apply univariate Cox regression and rank the genes based on the Cox p-value.
}
\usage{
featureselection(gnExpMat, survivaltime, censor, method = "none",gn.nb)
}
\arguments{
  \item{gnExpMat}{
Matrix of gene expression data.
}
  \item{survivaltime}{
Vector of survival time.
}
  \item{censor}{
Vector of censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
 \item{method}{
	A character string specifying the feature selection method: "none" for top-ranking or one of the adjusting methods specified by the p.adjust function.
}
\item{gn.nb}{
Number of genes to select for gene signature when \code{method}="none".
}
}
\details{
In top-ranking, genes are selected based on univariate Cox P-value ranking using
 the coxph function in the R survival package. In this feature selection method,
 the genes are ranked based on their likelihood ratio P-value and the top-\code{gn.nb} ranked genes with the smallest P-values are retained as the gene signature.

The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".
}
\references{
Yasrebi H,  Sperisen P,  Praz V,  Bucher P, 2009 Can Survival Prediction Be Improved By Merging Gene Expression Data Sets?. PLoS ONE 4(10): e7431. doi:10.1371/journal.pone.0007431.}
\value{
A list of two vectors, the Cox coefficients and Cox p-values.
}

\author{
Haleh Yasrebi
}
\section{Warning }{This function is not called by the user directly.} 
