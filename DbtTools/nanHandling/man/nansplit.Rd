\name{nansplit}
\alias{nansplit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Nansplit }
\description{
 Split matrix into rows with NaNs and without NaNs
}
\usage{
nansplit(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a matrix }
}

\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{ complete }{a matrix from the rows without NaNs}
  \item{ withNaN }{a matrix from the rows with NaNs, sorted ascending by number of NaNs}
  ...
}
\author{ Zornitsa Manolova }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ split }
