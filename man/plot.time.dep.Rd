\name{plot.time.dep}
\alias{plot.time.dep}
\title{
Plot time-dependent ROC curves from 0 to 120 months.
}
\description{
Plot time-dependent ROC curves for the testing set from 0 to 120 months. As the clinical trials are carried out up to 10 years, the maximum time point is limited to 120 months.
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

