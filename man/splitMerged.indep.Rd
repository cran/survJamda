\name{splitMerged.indep}
\alias{splitMerged.indep}
\title{
Merge the data sets by ComBat or Z-score1 normalization and apply independent validation.
}
\description{
The data sets are either adjusted by Z-score1 normalization or ComBat. In Z-score1 normalization, all data sets are first Z-score normalized and then, merged together. The selection of data sets for the training and testing sets is performed before the application of independent validation. 
}
\usage{
splitMerged.indep(geno.files,files, lst, i, j, method,gn.nb,perf.eval)
}
\arguments{
 \item{geno.files}{
a vector of character containing the names of gene expression data files.
}
 \item{files}{
A vector of character strings containing the names of the expression files.
}
  \item{lst}{
A list of two objects, (i) gene expression data and (ii) list of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
  \item{i}{
Index of the names of the expression files composing the training set.
}
  \item{j}{
Index of the name of the expression file used as the testing set.
}
 \item{method}{
		A character string specifying the feature selection method: "none" for top-ranking or one of the adjusting method specifying by the p.adjust function
}
\item{gn.nb}{
Number of genes to select for gene signature when \code{method}="none".
} 
\item{perf.eval}{
A string taking one the values, "auc", "cindex", "bsc".
} 
}


\details{
The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".

If \code{perf.eval} == "auc", time-dependent AUC and hazard ratio are used as the measure of performance, \code{perf.eval} == "cindex", concordance index defined in the \code{survcomp} package or \code{perf.eval} == "bsc", brier score defined in the \code{survcomp} package is used.
}
\references{
Yasrebi H,  Sperisen P,  Praz V,  Bucher P, 2009 Can Survival Prediction Be Improved By Merging Gene Expression Data Sets?. PLoS ONE 4(10): e7431. doi:10.1371/journal.pone.0007431.
}
\value{
None.
}

\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.} 

