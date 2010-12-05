\name{comb.surv.censor}
\alias{comb.surv.censor}

\title{
Merge survival times and censoring status.
}
\description{
Merge vectors of survival time and censoring status of different data sets for joint analysis.
}
\usage{
comb.surv.censor(geno.files,index,surv.data)
}

\arguments{
  \item{geno.files}{
Vector of character strings containing the names of expression files.

} 
\item{index}{
Index of the data files in \code{geno.files} to be combined.
}

  \item{surv.data}{
List of two vectors, survival time and censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}
}

\value{
A list of two vectors, combined survival time and censoring status.
}

\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.} 

