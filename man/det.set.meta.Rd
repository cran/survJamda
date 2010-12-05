\name{det.set.meta}
\alias{det.set.meta}

\title{
Split data for meta analysis.
}
\description{
Split data into the training and testing sets for meta analysis.
}
\usage{
det.set.meta(i, j, geno.files, surv.data, method)
}

\arguments{
  \item{i}{
A vector of character strings consisting of the names of the expression files used for the training set.
}
  \item{j}{
A character string specifying the name of the expression file used for the testing set.
}
  \item{geno.files}{
A vector of character strings consisting of the names of the expression files.
}
  \item{surv.data}{
A list of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
 \item{method}{
		A character string specifying the feature selection method: "none" for top-100 ranking or one of the adjusting methods specified by the p.adjust function}
}

\details{
In top-ranking, genes are selected based on univariate Cox P-value ranking using
 the coxph function in the R survival package. In this feature selection method,
 the genes are ranked based on their likelihood ratio P-value and the top-100 ranked genes with the smallest P-values are retained as the gene signature.

The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".
} 
\value{
None.
}

\author{
Haleh Yasrebi
}
\keyword{Meta analysis}
