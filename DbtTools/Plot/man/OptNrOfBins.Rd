\name{OptNrOfBins}
\alias{OptNrOfBins}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Optimal Number Of Bins }
\description{
  Calculation of the optimal number of bins for a histogram.
  After Keating/Scott 99
}
\usage{
OptNrOfBins(Data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{ Data }
}
\details{
  Used in hist(data,OptimalNrOfBins)
}
\value{
  \item{optNrOfBins}{The best possible number of bins. Not less than 10 though}
}
\references{  
David W. Scott Jerome P. Keating: „A Primer on Density Estimation for the Great Home Run Race of ’98”, STATS #25, 1999, pp 16-22.
}
\author{databionics UnivMarburg}
%\note{  }
%\seealso{  }
%\examples{  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ histogram }
%\keyword{ optimal }
%\keyword{ number }
%\keyword{ bins }