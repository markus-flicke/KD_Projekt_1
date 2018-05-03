\name{qqfit}
\alias{qqfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ QQFit }
\description{
  Quantile/Quantile-Plot with best-fit-line
}
\usage{
qqfit(x, y, yint)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Original points vectors of length (1:NumberOfPoints) }
  \item{y}{ Original points vectors of length (1:NumberOfPoints) }
  \item{yint}{ y values  such that (x,yint) are the poihnts of regression line }
}
\details{     }
\value{     
  \item{delta}{The differeces: yint-y}
  \item{meanAbsDelta}{sum(abs(Delta))/NumberOfPoints;}
}
\references{  }
\author{databionics UnivMarburg}
\note{  }
\seealso{  }
\examples{   }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }