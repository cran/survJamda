\name{featureselection.meta}
\alias{featureselection.meta}
\title{
Feature selection for meta analysis
}
\description{
Apply univariate Cox regression and aggregate gene Z-scores.
}
\usage{
featureselection.meta(gnExpMat, survivaltime, censor)
}

\arguments{
  \item{gnExpMat}{
Matrix of gene expression data.
}
  \item{survivaltime}{
Vector of survival time.
}
  \item{censor}{
Vector of censoring status.
}
}

\value{
Vector of gene Z-scores.
}

\author{
Haleh Yasrebi
}
\section{Warning }{This function is not called by the user directly.} 
\keyword{Meta analysis}
