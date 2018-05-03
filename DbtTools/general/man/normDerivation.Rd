\name{normDerivation}
\alias{normDerivation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{First derivative of the Gaussian distribution}
\description{
  Calculates the first derivative of the Gaussian distribution (mexican hat function).
}
\usage{
normDerivation(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Calculate the first derivative of the Gaussian distribution
  for the values \code{x}.}
}
\details{
  Maximum at \code{x} == -1, then y == -1 and normDerivation = 1/sqrt(2*pi))*exp(-0.5) = 0.2420.

  Minimum at \code{x} == 1, then y == 1 and normDerivation = -1/sqrt(2*pi))*exp(-0.5) = 0.2420.
}
\value{
  Retruns a vector of y-values based on the input vector \code{x}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Sascha Feld}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{norm2Derivation}}, \code{\link{norm3Derivation}}}
\examples{
# Create data.
x <- rnorm(10)*10
normDerivation(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{normDerivation}
\keyword{first derivative of the Gaussian distribution}
\keyword{Gaussian distribution}
\keyword{distribution}
\keyword{derivative}