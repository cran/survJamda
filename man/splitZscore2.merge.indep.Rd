\name{splitZscore2.merge.indep}
\alias{splitZscore2.merge.indep}
\title{
Merge data sets by Z-score2 normalization and assess the performance by independent validation.
}
\description{
Z-score2 normalization is performed as follows: First, the data sets are selected for the training and testing sets. Suppose there are S data set. Then, in S iteration, S-1 data sets selected as the training set and the remaining set as the testing set until all data sets are used in the training and testing sets. The data sets composing the training set are merged together and the merged data set is then Z-score normalized. The testing set is independently adjusted by Z-score normalization.
}
\usage{
splitZscore2.merge.indep(common.gene, geno.files, surv.data, lst, i, j, 
method,gn.nb)
}

\arguments{
  \item{common.gene}{
A vector of character strings containing the names of the genes common to the datasets composing the training set.
}
  \item{geno.files}{
A vector of character strings containing the names of the expression files.
}
  \item{surv.data}{
A list of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
  \item{lst}{
The list of two objects, the gene expression data and \code{surv.data}.
}
  \item{i}{
Index of the names of the expression files composing the training set.
}
  \item{j}{
Index of the name of the expression file used as the testing set.
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
\value{
None.
}

\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.} 
\keyword{Z-score normalization}

