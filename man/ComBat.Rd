\name{ComBat}
\alias{ComBat}
\title{
ComBat-adjusted microarray gene expression data
}
\description{
Compute ComBat-adjusted microarray gene expression data.
}
\usage{
ComBat(expression_xls, sample_info_file, type = "txt", write = TRUE, 
covariates = "all", par.prior = TRUE, filter = FALSE, skip = 0, 
prior.plots = TRUE)
}
\arguments{
  \item{expression_xls}{
A character string specifying gene expression file.
}
  \item{sample_info_file}{
A character string specifying sample file.
}
  \item{type}{
A character string specifying the type of the file, "txt" or "csv".
}
  \item{write}{
A Boolean variable indicating whether the output (adjusted data) should be written into a file.
}
  \item{covariates}{
A vector of integers or "all" if all covariates should be used. \code{covariates=all} will use all of the columns in your sample info file in the modeling (except array/sample name), if you only want use a some of the columns in your sample info file, specify these columns here as a vector (you must include the Batch column in this list).
}
  \item{par.prior}{
A Boolean character indicating whether the parametric adjustment should be applied.
}
  \item{filter}{
A Boolean variable indicating whether presence/absence call is used in the gene expression file.
}
  \item{skip}{
An integer value indicating the number of columns that contain the gene names. skip = 1 implies the first expression values start from column 2.
}
  \item{prior.plots}{
A Boolean variable indicating whether the prior plots should be given where black is a kernel density estimate of the batch effects. Quantile-quantile plots are also included. If the red and black lines do not match up well, use the nonparametric adjustment. 
}
}

\value{
Matrix of adjusted expression data.
}
\references{
W. Johnson E., L. Chen, Rabinovic, and A. Adjusting batch effects in microarray
expression data using Empirical Bayes methods. Biostatistics, 8(1):118-127, January2007. ISSN 1465-4644. 

}
\author{
WE Johnson
}
\section{Warning }{This function is not called by the user directly.} 
\keyword{ComBat}
