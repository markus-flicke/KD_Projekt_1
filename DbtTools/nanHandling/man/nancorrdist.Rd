\name{nancorrdist}
\alias{nancorrdist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Nancorrdist }
\description{
   One minus the sample correlation between points (treated as sequences of values).
   NaN values are disregarded
   Euclidean distances between vectors containing NaN output
}
\usage{
nancorrdist(Data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{ a matrix }
}
\details{
  use \code{\link{nancor}}
}
\value{
  \item{distanceMatrix }{Description of 'comp2'}
  \item{vectorOfDistances }{Description of 'comp1'}
}
\author{ Zornitsa Manolova }

\seealso{See Also as \code{\link{nancor}} }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }

