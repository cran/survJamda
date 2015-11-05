\name{plot.roc.curves}
\alias{plot.roc.curves}
\title{
Plot ROC curves of the testing set normalized by a joint analysis method.
}
\description{
Plot ROC curves of the testing set normalized by a joint analysis method, Z-score2, Z-score1 or ComBat.
}

\arguments{
  \item{surv}{
	The vector of survival times of the testing set.
}
  \item{censor}{
	The vector of censoring status of the testing set.
}
  \item{lp}{
	The vector of biomarkers derived from the testing set.
}
  \item{test}{
	The matrix of the testing set.
}
  \item{file.name}{
	The name of the file of the testing set.
}
  \item{col}{
	The color of the ROC curve.
}
  \item{normalization}{
	The normalization method, Z-score2, Z-score1 or ComBat.
}
}
\value{
None.
}
\references{
Haleh Yasrebi, Comparative study of joint analysis of microarray gene expression data in survival prediction and risk assessment of breast
cancer patients, Brief Bioinform, 2015 doi:10.1093/bib/bbv092. 
}
\author{
	Haleh Yasrebi
}
\section{Warning}{This function is not called by the user directly.}

