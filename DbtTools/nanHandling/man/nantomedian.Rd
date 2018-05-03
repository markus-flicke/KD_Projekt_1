\name{nantomedian}
\alias{nantomedian}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ NaN to Median }
\description{
  Substitute any values in z by the median value
}
\usage{
nantomedian(z)
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

\seealso{  See Also as \code{\link{nantovalue}}, ~~~ }
\keyword{ NA }
\keyword{ NaN }
\keyword{ replace }
\keyword{ substitute }