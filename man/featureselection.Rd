\name{featureselection}
\alias{featureselection}
\title{
Apply a feature selection
}
\description{
Apply univariate Cox regression and rank the genes based on the Cox p-value.
}
\usage{
featureselection(gnExpMat, survivaltime, censor)
}
\arguments{
  \item{gnExpMat}{
Matrix of gene expression data.
}
  \item{survivaltime}{
Vector of survival time.
}
  \item{censor}{
Vector of censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
}
\references{
Yasrebi H,  Sperisen P,  Praz V,  Bucher P, 2009 Can Survival Prediction Be Improved By Merging Gene Expression Data Sets?. PLoS ONE 4(10): e7431. doi:10.1371/journal.pone.0007431.}
\value{
A list of two vectors, the Cox coefficients and Cox p-values.
}

\author{
Haleh Yasrebi
}
\section{Warning }{This function is not called by the user directly.} 
