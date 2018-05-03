\name{cauchyinv}
\alias{cauchyinv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Cauchy Inverse / Die Funktion plotet nur, ist die in dbt.plot auch vorhanden? }
\description{
 Inverse of the Cauchy cumulative distribution function (cdf), x= M + S*tan(pi*(p-0.5)).
}
\usage{
cauchyinv(p, ml=0.0, md=1.0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{ a vector with percentiles, the velaues muss be betwen 0 and 1 }
  \item{ml}{ meassury of location, default=0 }
  \item{md}{ meassury of dispersion, muss be positive, default=1  }
}

\author{ Zornitsa Manolova }

\seealso{  \code{\link{cauchycdf}} }
\examples{
p<-seq(0.1,0.9,0.01)
plot(cauchyinv(p))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cauchy }
\keyword{ plot }
\keyword{ inverse }
