\name{excl.missing.single.indep}
\alias{excl.missing.single.indep}
\title{
Exclude missing samples prior to independent validation
}
\description{
Exclude samples with missing survival time points prior to the application of independent validation to single data sets.
}
\usage{
excl.missing.single.indep(geno.files,ind,surv.data, common.gene)
}

\arguments{
  \item{geno.files}{
A vector of character strings containing the names of the expression files.
}
\item{ind}{
Index of expression files in \code{geno.files} to combine.
}
  
\item{surv.data}{
The list of two vectors, survival time and censoring status.
}
  \item{common.gene}{
A vector of character strings containing the names of the genes common to all data sets.
}
}

\value{
A list of two objects, (i) Matrix of gene expression data of the patients with no missing survival time. (ii) The list of two vectors, survival time and censoring status of the patients with no missing survival time points.
}

\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.} 

