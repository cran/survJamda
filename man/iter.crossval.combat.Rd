\name{iter.crossval.combat}
\alias{iter.crossval.combat}
\title{
Merge data set by ComBat within cross-validation.
}
\description{
Assess the performance of the gene signatures derived from the merged data set adjusted by ComBat by ten iterations of cross-validation. 
}
\usage{
iter.crossval.combat(data, surv, censor, batchID, ngroup = 10, plot.roc = 0,
 method = "none", gn.nb = 100)
}
\arguments{
  \item{data}{
Matrix of gene expression data.
}
  \item{surv}{
Vector of survival times.
}
  \item{censor}{
Vector of censoring status. 1 = event occurred, 0 = censored.
}
\item{batchID}{
For a given data set, the batch id can be an integer or the name of the data set. The batch id must be the same for all samples or arrays of a data set. 
}  
\item{ngroup}{
An integer specifying the number of cross-validation folds.
}
  \item{plot.roc}{
A integer (0 or 1) indicating whether the ROC curves should be plotted.
}
\item{method}{
		A character string specifying the feature selection method: "none" for top-ranking or one of the adjusting methods specified by the p.adjust function.
} 
  \item{gn.nb}{
An integer specifying the number of genes to select.
}
}
\details{
The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".

If the user wants to apply his own feature selection method, he should define his function with the same number of parameters as the defined feature selection function of the package, i.e. \code{featureselection}.

ROC curves are the plots of the mean of true positives (sensitivity) and the mean of false positives (1-specificity) over \code{ngroup} folds of cross-validation.
}
\references{
Yasrebi H,  Sperisen P,  Praz V,  Bucher P, 2009 Can Survival Prediction Be Improved By Merging Gene Expression Data Sets?. PLoS ONE 4(10): e7431. doi:10.1371/journal.pone.0007431.
}
\value{
Arithmetic mean of AUC +/- standard deviation (AUC) and geometric mean of HR(CI).
}

\author{
Haleh Yasrebi
}

\seealso{
\code{\link{iter.crossval}} 
}
\examples{
require(survJamda.data)

data(gse4335)
data(gse4335pheno)

data(gse1992)
data(gse1992pheno)

common.gene = intersect(colnames(gse4335), colnames(gse1992))

data = rbind(gse4335[,common.gene], gse1992[,common.gene])
surv = c(gse4335pheno[,6],gse1992pheno[,19])
censor = c(gse4335pheno[,5],gse1992pheno[,18])

# An integer is used as batchID 
batchID = rep(1,nrow(gse4335))
batchID = c(batchID,rep(2,nrow(gse1992)))

#Or the name of the data sets is used as batch ID
#batchID = rep("gse4335",nrow(gse4335))
#batchID = c(batchID,rep("gse1992",nrow(gse1992)))

#And run the following script:
#iter.crossval.combat(data, surv,censor, batchID)

## The function is currently defined as
function (data, surv, censor, batchID, ngroup=10, plot.roc = 0, method = "none",
gn.nb = 100){

        require(survival)
        require(survivalROC)

        if(!exists("batchID"))
                stop("\rSet batchID", call.=FALSE)
	
        niter = ifelse(ngroup == length(surv), 1,10)
        res = NULL

        file.name=deparse(substitute(data)) 
        if (plot.roc)
                init.plot(file.name)

        data =data[!is.na(surv),]
        censor= censor[!is.na(surv)]
        surv= surv[!is.na(surv)]

        cat ("Iteration\tAUC\tHR(CI)\t\tP-val\n")
        for (i in 1:niter){
                new.lst = cross.val.combat(data, surv, censor,method = "none", 
                gn.nb, plot.roc, ngroup, i)
                res = rbind (res, new.lst)
        }

        if(ngroup != length(surv)){
                cat ("Avg AUC+/-SD\tHR(CI)\n")
                if (plot.roc)
                legend (0.55,0.1, legend = paste("AUC+/-SD =", sprintf("\%.2f",
                as.numeric(mean(res[,1],na.rm = TRUE))), "+/-", sprintf("\%.2f",
                sd (res[,1],na.rm = TRUE)), sep = " "), bty = "n")
		
                cat (sprintf("\%.2f",as.numeric(mean(res[,1], na.rm = TRUE))), 
                "+/-", sprintf("\%.2f",sd (res[,1],na.rm = TRUE)), "\t", 
                gm(res[,2]), "(", sprintf("\%.2f",ci.gm(res[,2])[1]), "-", 
                sprintf("\%.2f",ci.gm(res[,2])[2]), ")\n", sep = "")
        }
}
}
\keyword{survival}
\keyword{survivalROC}
\keyword{ComBat}
\keyword{Plot}
