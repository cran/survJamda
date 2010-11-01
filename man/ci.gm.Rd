\name{ci.gm}
\alias{ci.gm}

\title{
Confidence interval of a Geometric mean
}
\description{
Calculate the Confidence Interval (CI) of a geometric mean.
}
\usage{
ci.gm(x)
}

\arguments{
  \item{x}{
Vector of numeric values. For example, a vector of HRs.
}
}

\value{
Vector of the CI of a geometric mean.
}

\author{
Haleh Yasrebi
}
\seealso{
 \code{\link{gm}}
}

\examples{
v = c(1.5,2.5,7,4)
ci.gm(v)

## The function is currently defined as
function(x){
          gm1 = mean(log(x), na.rm = T)
          cil = exp(gm1-(1.96*(sd(log(x), na.rm = T)/sqrt(length(x)))))
          ciupp = exp(gm1+(1.96*(sd(log(x), na.rm = T)/sqrt(length(x)))))
          vec = c(round(cil,2), round(ciupp,2))
          return (vec)
}
}
