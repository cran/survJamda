\name{splitZscore2.auc.plot}
\alias{splitZscore2.auc.plot}
\title{
Z-score2 normalization prior to AUC plot.
}
\description{
For independent validation, merge survival time and censoring status of the data sets composing the training set and apply the Z-score normalization prior to the plot of AUC.
}
\usage{
splitZscore2.auc.plot(common.gene, geno.files, surv.data, lst, i, j,
 col, method, time.dep)
}

\arguments{
  \item{common.gene}{
A vector of character strings containing the names of the genes common to the all data sets.
}
  \item{geno.files}{
A vector of character strings containing the names of gene expression files.
}
  \item{surv.data}{
A list of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
  \item{lst}{
The list of two objects, (i) matrix of gene expression data and (ii) list of two vectors, survival time and censoring status.
}
  \item{i}{
Index of the file names composing the training set.
}
  \item{j}{
Index of the file name used as the testing set.

}
  \item{col}{
Color of ROC curve.
}
 \item{method}{
		A character string specifying the feature selection method: "none" for top-100 ranking or one of the adjusting methods specified by the p.adjust function.} 
\item{time.dep}{
		An integer 0 or 1, 1 to plot time-dependent ROC curves for different time points and 0 for no plot}

}
\details{
Z-score2 normalization is performed as follows: First, the data sets are selected for the training and testing sets. Suppose there are S data set. Then, in S iteration, S-1 data sets are selected for the training set and the remaining set selected as the testing set until all data sets are used in the training and testing sets. The data sets composing the training set are merged together and the merged data set is then Z-score normalized. The testing set is independently adjusted by Z-score normalization.

In top-ranking, genes are selected based on univariate Cox P-value ranking using the coxph function in the R survival package. In this feature selection method, the genes are ranked based on their likelihood ratio P-value and the top-100 ranked genes with the smallest P-values are retained as the gene signature.

The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".
}
\value{
None.
}
\author{
Haleh Yasrebi
}

\section{Warning }{This function is not called by the user directly.} 
\keyword{Z-score normalization}
\keyword{Plot}
