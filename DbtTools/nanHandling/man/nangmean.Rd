\name{nangmean}
\alias{nangmean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Geometrical Mean }
\description{
  Compute geometrical mean value with NAs/NaNs und zero handling.
}
\usage{
nangmean(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector }
}
\details{
  The issue ist always a single value. Matrices are not speacially treated
  to calculate the value column by column. 
}
\value{
  \item{m }{geometrical mean value}
}
\author{ Zornitsa Manolova }

\seealso{ \code{gmean} from the Package dbt.Statistics }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ mean }
