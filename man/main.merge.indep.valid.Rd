\name{main.merge.indep.valid}
\alias{main.merge.indep.valid}
\title{
Performance assessment of merged data sets by independent validation
}
\description{
Assess the performance of survival prediction derived from the merged data sets  by independent validation.
}
\usage{
main.merge.indep.valid(geno.files,surv.data,gn.nb=100,method = "none",
 normalization = "zscore1")
}

\arguments{
  \item{geno.files}{
Vector of character strings containing the names of gene expression files.
}
  \item{surv.data}{
List of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
\item{gn.nb}{
Number of genes to select for gene signature when \code{method}="none".
} 
\item{method}{
		A character string specifying the feature selection method: "none" for top-ranking or one of the adjusting methods specified by the p.adjust function.
}
  \item{normalization}{
A character string, "combat", "zscore1" or "zscore2".
}
}
\details{
In Z-score1 normalization, all data sets are Z-score normalized separately and then, the data sets composing the training set are merged together. The remaining set is used as the testing set. This process is continued S times (S being the number of data sets) until all data sets are used in the training and testing sets. 

In Z-score2 normalization, the data sets are selected for the training and testing sets. Suppose there are S data sets. Then, in S iteration, S-1 data sets are used for the training set and the remaining set used as the testing set. This process is continued until all data sets are used in the training and testing sets. In each iteration, the data sets composing the training set are first merged together and the merged data set is then Z-score normalized. The testing set is independently adjusted by Z-score normalization.

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
#main.merge.indep.valid(geno.files,surv.data)

function(geno.files,surv.data,method = "none", normalization= "zscore1")
{
        require(survival)
        require(survivalROC)

        if (length(geno.files) < 3) 
                stop ("\rThere should be minimum 3 data sets", call. = FALSE)

        if(normalization == "combat")
                batchID = det.batchID()

        if (!is.element(normalization, c("zscore","combat")))
                stop("\rnormalization = \"zscore\" or normalization = \"combat\"", 
                call.=FALSE)

       common.gene = colnames(get(geno.files[1]))
       for (i in 2:length(geno.files))
              common.gene = intersect(common.gene, colnames(get(geno.files[i])))

       curr_set = 1:length(geno.files)

       for (y in curr_set){
              x = setdiff(curr_set, y)
              prep = get(paste("prep",normalization, sep = ""))
              lst = prep(common.gene,geno.files,surv.data,x,y)	

              if (normalization == "zscore1" || normalization == "combat")
                     splitMerged.indep (geno.files,lst, x, y, method)
              else
                     splitZscore2.merge.indep (common.gene,geno.files,surv.data,
                     lst, x, y, method)	
       }
}
}

