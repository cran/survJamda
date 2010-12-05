\name{pool.zscores}
\alias{pool.zscores}
\title{
Combine data for meta analysis.
}
\description{
Combine the expression data, survival data (survival time and censoring status) and gene Z-scores for meta analysis.
}
\usage{
pool.zscores(common.gene, s, geno.files, surv.data)
}
\arguments{
  \item{common.gene}{
A vector of character strings containing the name of the genes common to the data sets composing the training set.
}
  \item{s}{
A vector of integers specifying the index of the expression files composing the training set.
}
  \item{geno.files}{
A vector of character strings consisting of the names of the expression files.

}
  \item{surv.data}{
A list of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.

}
}

\value{
None.
}

\author{
Haleh Yasrebi
}
\section{Warning }{This function is not called by the user directly.} 
