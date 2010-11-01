\name{cross.val.combat}
\alias{cross.val.combat}
\title{
Cross validation with ComBat adjustment
}
\description{
Assess the performance of the gene signatures derived from the merged data set adjusted by ComBat in cross-validation.
}
\usage{
cross.val.combat(x, y, censor, batchID, method, gn.nb, plot.roc, ngroup, iter)
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
 \item{batchID}{
Vector containing the batch ID of the data set \code{x}. The batch ID of the data sets composing the matrix \code{x} should be in the same order of the component data sets. For a given data set, the batch id can be an integer or the name of the data set. The batch id must be the same for all samples or arrays of a data set. 

}
  \item{method}{
A character string specifying the feature selection method: "none" for top-ranking (top-100 ranking by default) or one of the adjusting methods specified by the p.adjust function.
}
  \item{gn.nb}{
An integer variable specifying the number of genes to select. The default is 100.
}
  \item{plot.roc}{
An integer specifying whether the ROC curves should be plotted or not (1 or 0).

}
  \item{ngroup}{
An integer variable specifying the number of cross-validation folds. The default is 10.
}
  \item{iter}{
An integer variable specifying the current number of iteration.
}
}
\details{
The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".

ROC curves are the plots of the mean of true positives (sensitivity) and the mean of false positives (1-specificity) over \code{ngroup} folds of cross-validation.
}

\references{
Yasrebi H,  Sperisen P,  Praz V,  Bucher P, 2009 Can Survival Prediction Be Improved By Merging Gene Expression Data Sets?. PLoS ONE 4(10): e7431. doi:10.1371/journal.pone.0007431.

}
\value{
Arithmetic mean of AUC +/- standard deviation and geometric mean of HR(CI) generated from cross-validation.
}

\author{
Haleh Yasrebi
}
\seealso{
\code{\link{iter.crossval.combat}}
}

\keyword{survival}
\keyword{survivalROC}
\keyword{ComBat}
\keyword{Plot}
