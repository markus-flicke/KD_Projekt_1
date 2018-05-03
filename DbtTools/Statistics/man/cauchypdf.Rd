\name{cauchypdf}
\alias{cauchypdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Probability distribution function for Cauchy Distributuion }
\description{
   Return pdf from Cauchy Distribution and plot it. 
   Use Cauchy(stats)
}
\usage{
cauchypdf(Data, m=0, md=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{ a numeric vector or matrix  }
  \item{m}{ median, default=0 }
  \item{md}{ meassure of dispersion }
}
\value{
  \item{DataPDF}{ a vector with the length of the input data }
}
\author{ Zornitsa Manolova }
\seealso{ \code{\link{cauchycdf}},\code{\link{cauchyinv}} }
\examples{

Data<-seq(-4,4,0.01)
DataPDF<-cauchypdf(Data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cauchy }
\keyword{ cdf }
\keyword{ plot }
