\name{calPerformance.auc.plot}
\alias{calPerformance.auc.plot}
\title{
	Assess the performance obtained from the merged data set by independent validation
}
\description{
 	Identify a gene signature and reduce the gene set in the training and testing sets accordingly.
}
\usage{
	calPerformance.auc.plot(lst, train.ind, test.ind, file.name, col, method)
}
\arguments{
  	\item{lst}{
		List of two objects, the gene expression data matrix and a list of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
	}
  	\item{train.ind}{
     		Training set index.
	}
  	\item{test.ind}{
     		Testing set index.
	}
  	\item{file.name}{
     		The name of the expression file used as the testing set.
	}
  	\item{col}{
     		Color of ROC curve.
	}
  	\item{method}{
		A character string specifying the feature selection method: "none" for top-100 ranking or one of the adjusting methods specified by the p.adjust function.}
}


\details{
In top-ranking, genes are selected based on univariate Cox P-value ranking using the coxph function in the R survival package. In this feature selection method, the genes are ranked based on their likelihood ratio P-value and the top-100 ranked genes with the smallest P-values are retained as the gene signature.

The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".
}

\value{None.}

\section{Warning}{This function is not called by the user directly.}

\author{
  Haleh Yasrebi
}
