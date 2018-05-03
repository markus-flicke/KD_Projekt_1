\name{dbt_mad}
\alias{dbt_mad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Median Absolute Deviation }
\description{
  Compute the difference between the median absolute deviations for all values in your data. 
}
\usage{
dbt_mad(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ numeric data vector }
}
\value{
  \item{centerMad}{a vector with differences}
  \item{leftMad }{a vector with all negative differences}
  \item{rightMad }{a vector with all positive differences}
}

\author{ Zornitsa Manolova }

\seealso{ \code{\link{amad}}}
\examples{   

}
\keyword{ mad }
\keyword{ amad }
\keyword{ median }
