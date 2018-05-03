\name{nantonearest}
\alias{nantonearest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ NaN to nearest }
\description{
  Replace NaNs in z with values of nearest neighbor in Data
}
\usage{
nantonearest(z, Data, defined=rep(1,dim(Data)[2]))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ vector with NaNs }
  \item{Data}{ matrix of size nxd, d=number of column }
  \item{defined}{ vector of length d with 1 for columns to be used
                  every column where defined is not 1 is discarded.
                  If defined is missing all components are used }
}
\details{
 Use nantoknearest with k=1
}
\value{
  \item{ n }{vector with NaNs replaces}
}
\author{ Zornitsa Manolova }

\seealso{ See Also as \code{\link{nantoknearest}} }

\keyword{ NA }
\keyword{ NaN }% __ONLY ONE__ keyword per line
\keyword{ replace }
\keyword{ substitute }
