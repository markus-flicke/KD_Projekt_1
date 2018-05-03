\name{norm3Derivation}
\alias{norm3Derivation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Third derivative of the Gaussian distribution}
\description{
 Calculates the thrid derivative of the Gaussian distribution (mexican hat function).
}
\usage{
norm3Derivation(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Calculate the thrid derivative of the Gaussian distribution
  for the values \code{x}.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
    Retruns a vector of y-values based on the input vector \code{x}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Sascha Feld}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{normDerivation}}, \code{\link{norm2Derivation}}}
\examples{
# Create data.
x <- rnorm(10)*10
norm3Derivation(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{norm3Derivation}
\keyword{third derivative of the Gaussian distribution}
\keyword{Gaussian distribution}
\keyword{distribution}
\keyword{derivative}
