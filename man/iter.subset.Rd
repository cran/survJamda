\name{iter.subset}
\alias{iter.subset}
\title{
Performance evaluation by subsetting data sets in 100 iterations
}
\description{
A data set can be split to different subsets to determine if the performance derived from its subsets is improved by the increase of sample size. Each subset can then be split 100 times into the independent training and testing sets. The sample size of the training set is set by the user (20,50,...,up to 2/3 of the complete set) and the remaining samples are used for the testing set. A gene signature will be derived from the training set and assessed on the testing set.

The performance obtained from the larger subsets and ultimately, the complete set is more likely higher than the performance generated from the smaller subsets. If it is not the case, the performance improvement might have been retained by factors such as heterogeneity with respect to patient's cohort or tumor characteristics.
}
\usage{
iter.subset(data, surv, censor, method = "none", gn.nb = 50, train.nb = 100)
}

\arguments{
  \item{data}{
Matrix of gene expression data.
}
  \item{surv}{
Vector of survival times.
}
  \item{censor}{
Vector of censoring status.
}
	\item{method}{
		A character string specifying the feature selection method: "none" for top-ranking or one of the adjusting methods specified by the p.adjust function.}
  \item{gn.nb}{
An integer specifying the number of genes to select.
}
  \item{train.nb}{
An integer specifying the sample size of the training set.

}
}
\details{
In top-ranking, genes are selected based on univariate Cox P-value ranking using the coxph function in the R survival package. In this feature selection method, the genes are ranked based on their likelihood ratio P-value and the top-\code{gn.nb} ranked genes with the smallest P-values are retained as the gene signature.

The p.adjust function in the stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".
}
\value{
Mean of AUC +/- standard deviation of AUC, geometric mean of HR (CI).
}

\author{
Haleh Yasrebi
}

\examples{
data(gse4335)
data(gse4335pheno)
#The following script might be lengthy
#iter.subset(gse4335, gse4335pheno[,6],gse4335pheno[,5])
## The function is currently defined as
function (data, surv, censor,method = "none", gn.nb = 50, train.nb = 100){

        require (survival)
        require (survivalROC)

        data =data[!is.na(surv),]
        censor= censor[!is.na(surv)]
        surv= surv[!is.na(surv)]
	
        res = NULL
        iteration.nb = 100
	
        cat ("Iteration\tAUC\tHR(CI)\t\tP-val\n")

        for (i in 1:iteration.nb){
                new.lst = eval.subset(data, surv, censor,i, method, gn.nb, train.nb)
                res = rbind (res, new.lst)
        }

        cat ("Avg AUC+/-SD\tHR(CI)\n")

        cat (sprintf("\%.2f",mean(res[,1], na.rm = T)),  "+/-", 
        sprintf("\%.2f",sd (res[,1],na.rm = T)), "\t", 
        sprintf("\%.2f",gm(res[,2])), "(", 
        sprintf("\%.2f",ci.gm(res[,2])[1]), "-",
        sprintf("\%.2f",ci.gm(res[,2])[2]), ")\n", 
        sep = "")       
}
}
