\name{plotROC}
\alias{plotROC}
\title{
Plot ROC curves related to different time points.
}
\description{
Plot time-dependent AUC based on different survival time points.
}
\usage{
	plotROC(test.ind, all.surv, all.censor, lp, file.name, col)
}

\arguments{
  \item{test.ind}{
    	Index of testing set.

   }
  \item{all.surv}{
	Vector of combined survival times.
   }
  \item{all.censor}{
Vector of combined censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
   }
  \item{lp}{
Vector of linear predictor scores or markers. \code{lp} is the sum of gene expression values weighted by the Cox coefficients. 
   }
  \item{file.name}{
Vector of character strings containing the names of expression files.
   }
  \item{col}{
Color of ROC curve.
}
}
\value{
 ROC-curves plot.
}

\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.} 
\keyword{Plot}
