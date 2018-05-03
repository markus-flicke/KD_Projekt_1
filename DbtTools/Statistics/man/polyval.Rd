\name{polyval}
\alias{polyval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Polynomial evaluation}
\description{
  Matlab/Octave polyval() 
}
\usage{
polyval(v,x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{ numeric vector}
  \item{x}{ polyval evaluates v at each element of x }
}
\details{
  This Functions is from Statistic ToolBox for Matlab. See {\code{\link{http://www.mathworks.com/products/statistics/}}}
}
\value{
  \item{ y }{ y = polyval(p,x) returns the value of a polynomial of degree n evaluated at x }         
}
\author{ Robin Hankin }

\examples{
  polyval(c(1,2,1,2),1:10) #should be work 
}
\keyword{ evaluation }
\keyword{ polynomial }
