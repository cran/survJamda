\name{pred.time.indep.valid}
\alias{pred.time.indep.valid}

\title{
Prediction of survival time by independent validation.
}
\description{
Identify the gene list common to all single data sets and invoke the subsequent function \code{main.process}.
}
\usage{
pred.time.indep.valid(geno.files, surv.data, method = "none", time.dep = 0)
}

\arguments{
  \item{geno.files}{
A vector of character strings containing the names of gene expression files.
}
  \item{surv.data}{
A list of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
\item{method}{
		A character string specifying the feature selection method: "none" for top-100 ranking or one of the adjusting methods specified by the p.adjust function.}
\item{time.dep}{
		An integer 0 or 1, 1 to plot time-dependent ROC curves for different time points and 0 for no plot}
}

\value{
None.
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

geno.files = c("gse4335","gse1992","gse3143")
surv.data = list(c(gse4335pheno[,6],gse1992pheno[,19],gse3143pheno[,4]),
                 c(gse4335pheno[,5],gse1992pheno[,18],gse3143pheno[,3]))
#pred.time.indep.valid(geno.files, surv.data)

## The function is currently defined as
function(geno.files, surv.data)
{
	common.gene = colnames(get(geno.files[1]))
	for (i in 2:length(geno.files))
		common.gene = intersect(common.gene, colnames(get(geno.files[i])))

	par (mfrow = c(1,length(geno.files)))
	par(oma=c(2,2,length(geno.files),2))

	main.process (common.gene, geno.files, surv.data)
  }
}

