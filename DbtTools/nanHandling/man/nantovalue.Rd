\name{nantovalue}
\alias{nantovalue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ NaN to value }
\description{
  Substitute NaNs and NAs in z by value
}
\usage{
nantovalue(z, value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ a vector or matrix }
  \item{value}{ a value to substitute NaNs/NAs with}
}

\value{
  \item{ out }{ Data with NaNs replaced }
  
}
\author{ Zornitsa Manolova }
\seealso{  See Also as \code{\link{nantovalue}},\code{\link{nantozero}},
          \code{\link{nantomean}},\code{\link{nantomedian}},\code{\link{nantonearest}} }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ NA }
\keyword{ NaN }
\keyword{ replace }
\keyword{ substitute }
