\name{eval.subset}
\alias{eval.subset}
\title{
Performance evaluation derived from a subset of a data set 
}
\description{
Select a subset of a single data set and split it into the training and testing sets. Generate a gene signature from the training set and evaluate its performance on the testing set.
}
\usage{
eval.subset(x, y, censor, iter, method, gn.nb, train.nb)
}

\arguments{
  \item{x}{
Matrix of gene expression data.
}
  \item{y}{
Vector of survival time.
}
  \item{censor}{
Vector of censoring status.
}
  \item{iter}{
An integer specifying the current iteration.
}
\item{method}{
		A character string specifying the feature selection method: "none" for top-ranking or one of the adjusting methods specified by the p.adjust function.
} 
  \item{gn.nb}{
An integer specifying the number of genes to select.
}
  \item{train.nb}{
An integer specifying the sample size of the training set.
}
}
\details{
In top-ranking, genes are selected based on univariate Cox P-value ranking using the coxph function in the R survival package. In this feature selection method, the genes were ranked based on their likelihood ratio P-value and the top-\code{gn.nb} ranked genes with the smallest P-values were retained as the gene signature.

The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".
}

\value{
AUC and HR.
}

\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.} 
