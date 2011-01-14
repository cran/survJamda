\name{cal.cox.coef}
\alias{cal.cox.coef}
\title{
Cox coefficient calculation.
}
\description{
Calculate the Cox coefficients of covariates.
}
\usage{
cal.cox.coef (gnExpMat, survivaltime, censor)
}

\arguments{
  \item{gnExpMat}{
Matrix of gene expression data.
}
  \item{survivaltime}{
Vector of survival time.
}
  \item{censor}{
Vector of censoring status. 1 = event occurred, 0 = censored.
}
}
\references{
Yasrebi H,  Sperisen P,  Praz V,  Bucher P, 2009 Can Survival Prediction Be Improved By Merging Gene Expression Data Sets?. PLoS ONE 4(10): e7431. doi:10.1371/journal.pone.0007431.
}
\value{
Vector of Cox coefficients.
}

\author{
Haleh Yasrebi
}
