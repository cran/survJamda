\name{meta.main}
\alias{meta.main}
\title{
Meta analysis of survival data.
}
\description{
Meta analysis of microarray gene expression data for survival prediction.
}
\usage{
meta.main(geno.files, surv.data, method = "none")
}

\arguments{
  \item{geno.files}{
A vector of character strings containing the names of expression files.
}
  \item{surv.data}{
The list of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
 \item{method}{
		A character string specifying the feature selection method: "none" for top-100 ranking or one of the adjusting methods specified by the p.adjust function}
}


\details{
In top-ranking, genes are selected based on univariate Cox P-value ranking using the coxph function in the R survival package. In this feature selection method, the genes are ranked based on their likelihood ratio P-value and the top-100 ranked genes with the smallest P-values are retained as the gene signature.

The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".
}
\value{
AUC, HR(CI) and p-value.
}

\author{
Haleh Yasrebi
}

\examples{
require(survJamda.data)

data(gse4335)
data(gse3143)
data(gse1992)

data(gse4335pheno)
data(gse3143pheno)
data(gse1992pheno)

geno.files = c("gse4335", "gse3143","gse1992")
surv.data = list(c(gse4335pheno[,6],gse3143pheno[,4],gse1992pheno[,19]),
c(gse4335pheno[,5],gse3143pheno[,3],gse1992pheno[,18]))

#The following script might take some time
#meta.main(geno.files, surv.data)

## The function is currently defined as
function(geno.files,surv.data, method = "none")
{	
        options(warn=-1) 
        curr_set = 1:length(geno.files)
        for (y in curr_set){
                x = setdiff(curr_set, y)
                data.set.meta (x, y, geno.files,surv.data, method)
        }
}
}
\keyword{Meta analysis}
