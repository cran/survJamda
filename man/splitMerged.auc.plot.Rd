\name{splitMerged.auc.plot}
\alias{splitMerged.auc.plot}
\title{
Determine the indices of the training and testing sets.
}
\description{
Determine the indices of the training and testing sets prior to the plot of ROC curves.
}
\usage{
splitMerged.auc.plot(geno.files, lst, i, j, col, method, time.dep)
}

\arguments{
  \item{geno.files}{
A vector of character strings containing the names of the expression files.
}
  \item{lst}{
A list of two objects, (i) gene expression data and (ii) list of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
  \item{i}{
A vector of character strings containing the names of the expression files used for the training set.

}
  \item{j}{
A character string specifying the name of the expression file used as the testing set.
}
  \item{col}{
Color of ROC curve.
}
\item{method}{
		A character string specifying the feature selection method: "none" for top-100 ranking or one of the adjusting methods specified by the p.adjust function}
\item{time.dep}{
		An integer 0 or 1, 1 to plot time-dependent ROC curves for different time points and 0 for no plot}
}


\details{
The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".
}
\value{
None.

}

\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.} 

