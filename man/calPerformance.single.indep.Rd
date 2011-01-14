\name{calPerformance.single.indep}
\alias{calPerformance.single.indep}
\title{
Performance assessment on single data sets using independent validation
}
\description{
Assess the performance of the gene signatures on single data sets in pair-wise manner.
}
\usage{
calPerformance.single.indep(lst1, lst2, method,gn.nb,perf.eval)
}

\arguments{
  \item{lst1}{
A list of two objects, (i) the gene expression data and (ii) the list of survival time and censoring status of the data set used as the training set. In the censoring status vector, 1 = event occurred, 0 = censored.
}
  \item{lst2}{
A list of two objects, (i) the gene expression data and (ii) the list of survival time and censoring status of an independent data set used as the testing set. In the censoring status vector, 1 = event occurred, 0 = censored.
}
  \item{method}{
	A character string specifying the feature selection method: "none" for top-ranking or one of the adjusting methods specified by the p.adjust function.
}
\item{gn.nb}{
Number of genes to select for gene signature when \code{method}="none".
}
\item{perf.eval}{
A string taking one the values, "auc", "cindex", "bsc".
} 
}


\details{
In top-ranking, genes are selected based on univariate Cox P-value ranking using
 the coxph function in the R survival package. In this feature selection method,
 the genes are ranked based on their likelihood ratio P-value and the top-\code{gn.nb} ranked genes with the smallest P-values are retained as the gene signature.

The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".

If \code{perf.eval} == "auc", time-dependent AUC and hazard ratio are used as the measure of performance, \code{perf.eval} == "cindex", concordance index defined in the \code{survcomp} package or \code{perf.eval} == "bsc", brier score defined in the \code{survcomp} package is used.
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
