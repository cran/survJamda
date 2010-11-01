\name{proc.simulate}
\alias{proc.simulate}

\title{
Simulate survival data.
}
\description{
Simulation of survival data using a Weibull model. Two scenarios could be considered: Simulate gene expression values with and without correlation among genes. The aim is to determine if the prediction performance derived from the merged data set is mediated by the correlation among genes.  
}
\usage{
proc.simulate(tot.genes = 100, correlation = 0, gene.nb = 50, sample.nb = 400,
 beta.init = 0.5, shape = 1, scale = 1)
}

\arguments{
  \item{tot.genes}{
Number of total genes.
}
  \item{correlation}{
Correlation among genes (between 0 and 1).
}
  \item{gene.nb}{
Number of true survival genes to identify.
}
  \item{sample.nb}{
Sample size.
}
  \item{beta.init}{
Initial value for the Cox coefficients.
}
  \item{shape}{
Shape parameter of the Weibull model.
}
  \item{scale}{
Scale parameter of the Weibull model.
}
}
\details{
\code{beta} should not be close to zero. Otherwise, the true genes cannot be identified. Values between +/-0.2 and +/- 3 would be good choices. Shape and scale should be selected between 1 and 2 (integer, preferably). 
}
\value{
Mean of AUC +/- standard deviation(AUC), geometric mean of HR (CI).
}
\section{Warning}{
The user must enter a value for beta, scale and shape different from the initial values when (s)he is asked to do so. Otherwise, the old values are used for the second data set which will make the second data set have the same distribution as the first data set which is not desired. 
}  
\author{
Haleh Yasrebi
}

\examples{
#using the default parameters, run the following script:
#proc.simulate()

#other values to be used:
#correlation = 0.8
#number of genes: 10,1000,5000

## The function is currently defined as
function(tot.genes = 100, correlation = 0, gene.nb = 50, sample.nb = 400, 
beta.init = 0.5, shape = 1, scale = 1)
{
       require(survival)
       require(survivalROC)
       require(ecodist)

       d1 = generate.survival.data (gene.nb, tot.genes,sample.nb,beta.init, correlation, 
       shape, scale)
       old.beta.init = beta.init

       cat ("Enter a different value for beta for the second data set: 
       Example: beta.init = 0.1\n")
       beta.init = as.numeric(readline())
       old.shape = shape
       old.scale = scale

       cat ("Enter a different value for shape for the second data set. 
       Enter only a numeric value. Example: 2\n")
       shape = as.numeric(readline())
       if (!is.numeric(shape))
              shape = old.shape

       cat ("Enter a different value for scale for the second data set. 
              Enter only a numeric value. Example: 1.5\n")
       scale = as.numeric(readline())
       if (!is.numeric(scale))
              scale = old.scale

       d2 = generate.survival.data(gene.nb, tot.genes,sample.nb,beta.init, 
       correlation,shape, scale)

       eval.merge.simulate(d1,d2,tot.genes, gene.nb, zscore = 1)
}
}
