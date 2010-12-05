\name{iter.crossval}
\alias{iter.crossval}
\title{
Performance assessment of gene signatures by cross-validation.
}
\description{
Assess the performance of a gene signature derived from a single or merged data set by ten iterations of cross validation.
}
\usage{
iter.crossval(data, surv, censor, ngroup = 10, plot.roc = 0, method = "none", 
zscore = 0, gn.nb = 100, gn.nb.display = 0)
}

\arguments{
  \item{data}{
Matrix of gene expression data.
}
  \item{surv}{
Vector of survival time.
}
  \item{censor}{
Vector of censoring status. 1 = event occurred, 0 = censored.
}
  \item{ngroup}{
An integer specifying the number of cross-validation folds.
}
  \item{plot.roc}{
An integer specifying whether the ROC curves should be plotted or not (1 or 0).
}
\item{method}{
		A character string specifying the feature selection method: "none" for top-ranking or one of the adjusting methods specified by the p.adjust function} 
  \item{zscore}{
An integer specifying whether Z-score normalization should be applied or not (1 or 0). 1 if \code{data} is a merged data set or 0 if \code{data} is a single data set.
}
  \item{gn.nb}{
An integer specifying the number of genes to select for gene signature.
}
  \item{gn.nb.display}{
An integer specifying the number of selected genes to display.
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
Mean of AUC +/- standard deviation of AUC, geometric mean of HR(CI).
}

\author{
Haleh Yasrebi
}
\seealso{
\code{\link{iter.crossval.combat}}, 
}
\examples{
## Single data set
data(gse4335)
data(gse4335pheno)

#And run the following script:
#iter.crossval(gse4335, gse4335pheno[,6], gse4335pheno[,5])

## To observe the frequency of the CYB5D1 gene selection, run the following script:
#iter.crossval(gse4335, gse4335pheno[,6], gse4335pheno[,5], gn.nb =1, gn.nb.display = 1)

## Merged data set
data(gse4335)
data(gse4335pheno)

data(gse1992)
data(gse1992pheno)

common.gene = intersect(colnames(gse4335), colnames(gse1992))

data = rbind(gse4335[,common.gene], gse1992[,common.gene])
surv = c(gse4335pheno[,6],gse1992pheno[,19])
censor = c(gse4335pheno[,5],gse1992pheno[,18])

#And run the following script:
#iter.crossval(data, surv,censor, zscore=1)

## The function is currently defined as
function (data, surv, censor,ngroup=10, plot.roc = 0, method = "none", zscore=0,
gn.nb = 100,gn.nb.display = 0){

         require(survival)
         require(survivalROC)

         res = NULL

         file.name=deparse(substitute(data)) 
         if (plot.roc)
	         init.plot(file.name)

         data =data[!is.na(surv),]
         censor= censor[!is.na(surv)]
         surv= surv[!is.na(surv)]

         cat ("Iteration\tAUC\tHR(CI)\t\tP-val\n")
         for (i in 1:ngroup){
                  new.lst = cross.val.surv(data, surv, censor,ngroup, i, method, zscore,
                  gn.nb,gn.nb.display,plot.roc)
                  res = rbind (res, new.lst)
         }

         if(ngroup != length(surv)){
                  cat ("Avg AUC+/-SD\tHR(CI)\n")
                  if (plot.roc)
                           legend (0.55,0.1, legend = paste("AUC+/-SD =", sprintf("\%.2f",
                           as.numeric(mean(res[,1],na.rm = T))), "+/-", sprintf("\%.2f",
                           sd (res[,1],na.rm = T)), sep = " "),bty = "n")
                  cat (sprintf("\%.2f",as.numeric(mean(res[,1], na.rm = T))),  "+/-",
                  sprintf("\%.2f",sd (res[,1],na.rm = T)), "\t", gm(res[,2]), 
                  "(", sprintf("\%.2f",ci.gm(res[,2])[1]), "-", sprintf("\%.2f",
                  ci.gm(res[,2])[2]), ")\n", sep = "")
         }        
}
}
\keyword{survival}
\keyword{survivalROC}
\keyword{Plot}
