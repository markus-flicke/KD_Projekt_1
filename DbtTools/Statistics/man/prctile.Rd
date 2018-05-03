\name{prctile}
\alias{prctile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Matlab prctile() }
\description{
  Calculate the percentile of x
}
\usage{
percentiles(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ returns percentiles of the values in x. Is the x a vector so has quants the same size. Is x a matrix, 
        the ith row of quants contains the p(i)th percentiles of each column of x}
  \item{p}{ p is a scalar or a vector of percent values. 0<p<1 }
}
\details{
  This Functions is from Statistic ToolBox for Matlab. See {\code{\link{http://www.mathworks.com/products/statistics/}}}
}
\value{
  \item{ quants }{  vector of percentiles}         
}
\author{ Zornitsa Manolova }

\examples{
  
}
\keyword{ percentile }
\keyword{ quantile }