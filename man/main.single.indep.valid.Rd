\name{main.single.indep.valid}
\alias{main.single.indep.valid}
\title{
Independent validation of the performance of the gene signatures derived from single data sets.
}
\description{
Assess the performance of the gene signatures derived from the single data sets in pair-wise manner.
}
\usage{
main.single.indep.valid(geno.files, surv.data,normalization = "zscore",
 method = "none",gn.nb=100)
}
\arguments{
  \item{geno.files}{
A vector of character strings containing the names of expression files.
}
  \item{surv.data}{
The list of two vectors, survival time and censoring status.
}
  \item{normalization}{
A character string, "combat" or "zscore". 
}
\item{method}{
		A character string specifying the feature selection method: "none" for top-ranking or one of the adjusting methods specified by the p.adjust function.
}
\item{gn.nb}{
Number of genes to select for gene signature when \code{method}="none".
} 
}


\details{
The p.adjust function in the R stats package is used and all adjusted p-values not greater than 0.05 are retained if \code{method} != "none".
} 
\references{
Yasrebi H,  Sperisen P,  Praz V,  Bucher P, 2009 Can Survival Prediction Be Improved By Merging Gene Expression Data Sets?. PLoS ONE 4(10): e7431. doi:10.1371/journal.pone.0007431.
}
\value{
AUC, HR(CI) and p-value.
}

\author{
Haleh Yasrebi
}
\seealso{
\code{\link{znorm}}, \code{\link{ComBat}}
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

#The following script might  take some time
#main.single.indep.valid(geno.files, surv.data)
## The function is currently defined as
function(geno.files, surv.data,normalization = "zscore", method = "none")
{
       require(survival)
       require(survivalROC)
        
       if (!is.element(normalization, c("zscore","combat")))
                stop("\rnormalization = \"zscore\" or normalization = \"combat\"", 
                call.=FALSE)
	
       if(normalization == "combat")
                batchID = det.batchID()
  
        for (i in 1:length(geno.files)){
              for (j in 1:length(geno.files))
                     if (i != j){	
                            common.gene = intersect(colnames(get(geno.files[i])), 
                            colnames(get(geno.files[j])))
                            ds1 = excl.missing.single.indep(geno.files[i],
                            surv.data,common.gene)
                            ds2 = excl.missing.single.indep(geno.files[j],
                            surv.data,common.gene)
                            if (normalization == "combat")
                                   mat = prepcombat.single.indep(ds1$mat,ds2$mat)
                            else
                                   mat = prepzscore(ds1$mat,ds2$mat)	
				
                            i.adj = mat[1:nrow(ds1$mat),]
                            j.adj = mat[(nrow(ds1$mat)+1):nrow(mat),]
                            cat ("Train data set: ", geno.files[j], " Test data set: ", 
                            geno.files[i], "\n")
                            calPerformance.single.indep(list(mat=j.adj,phyno=ds2$phyno),
                            list(mat=i.adj,phyno=ds1$phyno), method=method)
                     }
       }
}
}
