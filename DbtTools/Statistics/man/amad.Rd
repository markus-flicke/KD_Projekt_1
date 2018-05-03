\name{amad}
\alias{amad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Adjusted Median Absolute Deviation }
\description{
  Compute the adjusted difference between the median absolute deviations for all values in your data. 
  The adjustemnt is such as that for a gaussian G amad(G) == std(G). If the input is a matrix
  the value will be calculate for every column.
}
\usage{
amad(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ numeric vector or matrix }
}
\value{
  \item{amad}{a vector with differences}
  \item{leftAmad }{a vector with all negative differences}
  \item{rightAmad }{a vector with all positive differences}
}

\author{ Zornitsa Manolova }

\seealso{ \code{\link{mad}}}
\examples{   

}
\keyword{ mad }
\keyword{ amad }
\keyword{ median }
