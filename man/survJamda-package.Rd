\name{survJamda-package}
\alias{survJamda-package}
\docType{package}
\title{
	Survival Prediction by Joint Analysis of Microarray Gene Expression Data
}
\description{
	Prediction of survival and risk assessment of patients using joint analysis of microarray gene expression data.	
}
\details{
	\tabular{ll}{
		Package: \tab survJamda\cr
		Type: \tab Package\cr
		Version: \tab 1.1.4\cr
		Date: \tab 2015-11-01\cr
		Depends: \tab survival, survivalROC, ecodist, survcomp, survJamda.data\cr
		License: \tab GPL (>= 2)\cr
		LazyLoad: \tab yes\cr
	}
}
\author{
	Haleh Yasrebi

	Maintainer: Haleh Yasrebi <hyasrebi@yahoo.com>
}

\references{
	Haleh Yasrebi, Comparative study of joint analysis of microarray gene expression data in survival prediction and risk assessment of breast
cancer patients, Brief Bioinform, 2015 doi:10.1093/bib/bbv092, PMID:26504096. 

	Yasrebi H. SurvJamda: an R package to predict patients' survival and risk assessment using joint analysis of microarray gene expression data. Bioinformatics. 2011 Apr 15;27(8):1168-9. doi: 10.1093/bioinformatics/btr103. Epub 2011 Mar 2. PubMed PMID: 21367873.

	Yasrebi H., Sperisen P., Praz V., Bucher P., Can Survival Prediction Be Improved By Merging Gene Expression Data Sets?, PLoS ONE 4(10): e7431. doi:10.1371/journal.pone.0007431, 2009, PMID: 21367873.

	Yasrebi, H. Prediction of survival and risk assessment using joint analysis of microarray gene expression data, PhD thesis, EPFL, Switzerland, doi:10.5075/Thesis 4494, 2010, PMID: 19851466.
}

\keyword{package}
\seealso{
\code{\link[survival]{coxph}},  
\code{\link[survivalROC]{survivalROC}},  
\code{\link[ecodist]{corgen}}, 
\code{\link[stats]{p.adjust}},
\code{\link[survcomp]{concordance.index}},
 \code{\link[survcomp]{sbrier.score2proba}}
}

