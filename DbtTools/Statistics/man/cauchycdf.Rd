\name{cauchycdf}
\alias{cauchycdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Comulative Density Function of Cauchy Distribution}
\description{
   Return cdf from Cauchy Distribution and plot it. 
   Use Cauchy(stats)
}
\usage{
cauchycdf(Data, m=0, md=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{a numeric vector}
  \item{m}{ median, default=0 }
  \item{md}{ meassure of dispersion,default =1 }
}
\value{
  \item{DataCDF}{}
}
\author{ Zornitsa Manolova }
\seealso{ \code{\link{cauchypdf}} }
\examples{
Data<-seq(-4,4,0.01)
DataCDF<-cauchycdf(Data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cauchy }
\keyword{ pdf}