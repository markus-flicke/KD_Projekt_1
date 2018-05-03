\name{symlogrnd}
\alias{symlogrnd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ LogNormal Random Numbers }
\description{
  Creates an array of lognormal distributed values
}
\usage{
symlogrnd(M, S, rows, cols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{ mean value by lognormal distribution}
  \item{S}{ standard deviation value by lognormal distribution }
  \item{rows}{ size of the random number arrays }
  \item{cols}{ size of the random number arrays }
}

\value{
  \item{randomSymLog }{lognormal distributed random numbers}
}
\author{ Zornitsa Manolova }
\seealso{ \code{\link{symlognSigmaMue}}}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ lognormal }
\keyword{ distribution }% __ONLY ONE__ keyword per line
