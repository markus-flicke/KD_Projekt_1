\name{norm2Derivation}
\alias{norm2Derivation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mexican hat function}
\description{
  Calculates the second derivative of the Gaussian distribution (mexican hat function).
}
\usage{
norm2Derivation(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Calculates the mexican hat function for the values \code{x}.}
}
\details{
  If \code{x} == 0, then norm2Derivation == 1-/sqrt(2*pi).

  If abs(\code{x}) == 1, then norm2Derivation == 0.
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
\seealso{\code{\link{normDerivation}}, \code{\link{norm3Derivation}}}
\examples{
# Create data.
x <- rnorm(10)*10
norm2Derivation(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{norm2Derivation}
\keyword{second derivative of the Gaussian distribution}
\keyword{Gaussian distribution}
\keyword{distribution}
\keyword{derivative}
\keyword{mexican hat function}