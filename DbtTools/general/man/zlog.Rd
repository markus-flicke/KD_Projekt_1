\name{zlog}
\alias{zlog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Logarithm for z-transformed values}
\description{
  Performes a z logarithm and creates z-transformed values, which won't change the
  negative values.
}
\usage{
zlog(x, b = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Values, \code{x} can be a matrix or a vector.}
  \item{b}{Change the base ( > 0) of the logarithm. Default is exp(1).}
}
\details{
  To undo the logarithm use the zinvlog() (see "seealso").
}
\value{
  Returns a vector/matrix with the z-logarithm of the values in \code{x}.
  All negative values won't be changed.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Sascha Feld}
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{zinvlog}}}
\examples{
# Create data.
x <- c(-1,1,7,8,2,2,9,3,3,3,9,-7)
x <- matrix(x,4,3)
zlog(x)
# Returns a matrix:    -1.0000    1.6931    2.0986
#                       1.0000    1.6931    2.0986
#                       2.9459    3.1972    3.1972
#                       3.0794    2.0986   -7.0000
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{z logarithm}
\keyword{z transformation}
\keyword{zlog}