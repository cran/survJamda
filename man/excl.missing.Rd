\name{excl.missing}
\alias{excl.missing}
\title{
Exclude missing samples
}
\description{
Exclude samples with missing survival times.
}
\usage{
excl.missing(mat,phyno)
}

\arguments{
  \item{mat}{
Matrix of gene expression data.
}
  \item{phyno}{
A list of two vectors, survival time and censoring status.
}
}

\value{
A list of two objects; (i) matrix of gene expression data of the patients with no missing survival times. (ii) the list of two vectors, survival time and censoring status of patients with no missing survival time time points.
}

\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.} 

