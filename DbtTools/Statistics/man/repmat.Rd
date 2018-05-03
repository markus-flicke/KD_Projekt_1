\name{repmat}
\alias{repmat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{REPMAT}
\description{
 The function is equal to repmat in Matlab
 See \code{\link{http://www.mathworks.de/access/helpdesk/help/toolbox/stats/index.html?/access/helpdesk/help/toolbox/stats/bq_w_hm.html&http://www.mathworks.de/products/statistics/}}
}
\usage{repmat(a,n,m)}
\arguments{
\item{a}{ numeric matrix }
\item{n}{ number of rows }
\item{m}{ number of columns }
}
\references{ \code{\link{http://cran.r-project.org/doc/contrib/R-and-octave.txt}} }
\author{ Robin Hankin }
\examples{
a <- matrix(1:4,2,byrow=TRUE)
repmat(a,2,3)
}
\keyword{ repmat }