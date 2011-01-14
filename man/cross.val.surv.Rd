\name{cross.val.surv}
\alias{cross.val.surv}
\title{
Cross validation with or without Z-score normalization
}
\description{
Assess the performance of the gene signatures derived from a single or merged data set by cross-validation.
}
\usage{
cross.val.surv(x, y, censor, ngroup, iter, method, zscore, gn.nb, 
gn.nb.display, plot.roc)
}
\arguments{
  \item{x}{
Matrix of gene expression data.
}
  \item{y}{
Vector of survival time.
}
  \item{censor}{
Vector of censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
  \item{ngroup}{
An integer specifying the number of cross-validation folds. The default is 10.
}
  \item{iter}{
An integer specifying the current number of iteration.
}
  \item{method}{
A character string specifying the feature selection method: "none" for top-ranking (top-100 ranking by default) or one of the adjusting methods specified by the p.adjust function.
}
  \item{zscore}{
An integer specifying whether Z-score normalization should be applied or not (1 or 0). 1 if the \code{data} is a merged data set and 0 if \code{data} is a single data set.
}
  \item{gn.nb}{
An integer specifying the number of genes to select. The default is 100.
}
  \item{gn.nb.display}{
An integer specifying the number of selected genes to display.
}
  \item{plot.roc}{
An integer specifying whether the ROC curves should be plotted or not (1 or 0).

}
}
\details{
The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".

If the user wants to apply his own feature selection method, he should define his function with the same number of parameters as the defined feature selection function of the package, i.e. \code{featureselection}.

ROC curves are the plots of the mean of true positives (sensitivity) and the mean of false positives (1-specificity) over \code{ngroup} folds of cross-validation.
}

\value{
 AUC and HR generated from cross-validation.
}

\author{
Haleh Yasrebi
}
\references{
Yasrebi H,  Sperisen P,  Praz V,  Bucher P, 2009 Can Survival Prediction Be Improved By Merging Gene Expression Data Sets?. PLoS ONE 4(10): e7431. doi:10.1371/journal.pone.0007431.
}
\keyword{survival}
\keyword{survivalROC}
\keyword{Plot}
