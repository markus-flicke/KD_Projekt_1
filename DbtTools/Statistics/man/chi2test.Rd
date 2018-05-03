\name{chi2test}
\alias{chi2test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Test Chi-squared Destributuion }
\description{
  The function transform the data and test if it is normal distributed.  
}
\usage{
chi2test(Data, Bins=50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{  a numeric vector or matrix }
  \item{Bins}{  number of bins to compare the empirical with the normal Distribution. }
}
\value{
  \item{ p }{ probability data is not normal distributed }
  
}
\author{ Zornitsa Manolova }

\seealso{ \code{\link{chi2testLS}} }
\examples{ 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chisquare }
\keyword{ distribution }

