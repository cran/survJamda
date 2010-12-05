\name{generate.survival.data}
\alias{generate.survival.data}
\title{
Generate survival data.
}
\description{
Generate survival data following a Weibull model with specified parameters such as Cox coefficients and correlation among genes. Then, identify a gene signature and assess its performance in cross-validation.
}
\usage{
generate.survival.data(gene.nb, tot.genes, sample.nb, beta.init, 
correlation, shape, scale)
}

\arguments{
  \item{gene.nb}{
The number of genes to select.
}
  \item{tot.genes}{
The total number of genes.
}
  \item{sample.nb}{
The total number of samples.
}
  \item{beta.init}{
Initial values for beta or Cox coefficients. The values between +/-0.5 to +/-3 are good choices.
}
  \item{correlation}{
Correlation among genes. The value should be between 0 and 1.
}
  \item{shape}{
Shape parameter of the Weibull model. Select a value between 1 and 5.
}
  \item{scale}{
Scale parameter of the Weibull model. Select a value between 1 and 5.
}
}

\value{
A list of three objects, (i) Matrix of simulated gene expression data, (ii) Vector of survival time and (iii) Vector of censoring status. In the censoring status vector, 1 = event occurred, 0 = censored.
}

\author{
Haleh Yasrebi
}
 \section{Warning }{This function is not called by the user directly.} 
\seealso{
\code{\link{proc.simulate}}
}
