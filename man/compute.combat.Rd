\name{compute.combat}
\alias{compute.combat}
\title{
Initiate ComBat adjustment
}
\description{
Call ComBat function for ComBat-adjustment of microarray gene expression data.
}
\usage{
compute.combat(fileGeno, fileSample)
}

\arguments{
  \item{fileGeno}{
A character string specifying the name of the gene expression file. Genes are in columns and samples are in rows. Column names should contain the gene names.
}
  \item{fileSample}{
A character string specifying the name of the file containing the sample or array names of gene expression data and batch ID.
}
}
\value{
ComBat-adjusted gene expression data matrix. Genes are organized in rows and samples are organized in columns.
}

\author{
Haleh Yasrebi
}

\section{Warning }{This function is not called by the user directly.} 
\keyword{ComBat}
