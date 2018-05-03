\name{nantomean}
\alias{nantomean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Nan to Mean }
\description{
  Substitute any NAs and NaNs by the mean value
}
\usage{
nantomean(z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ vector or matrix }
}
\details{
  If is the Data matrix, the mean velaues are calculated column by column
}
\value{
  \item{ out }{ vector/matrix without NAs/NaNs}
}
\author{ Zornitsa Manolova }

\seealso{  See Also as \code{\link{nantovalue}} }

\keyword{ NA }
\keyword{ NaN }
\keyword{ replace }
\keyword{ substitute }