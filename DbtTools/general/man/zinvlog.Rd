\name{zinvlog}
\alias{zinvlog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Undo logarithm of z-transformed values}
\description{
  Undo the logarithm performed by zlog.
}
\usage{
zinvlog(l, b = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{Values created by zlog.}
  \item{b}{Change this, if you changed the base of zlog. Default is exp(1).}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  Returns the original, unchanged data.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Sascha Feld}
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{zlog}}}
\examples{
# Create data.
x <- c(-1,1,7,8,2,2,9,3,3,3,9,-7)
x <- matrix(x,4,3)
y <- zlog(x)
zinvlog(y) == x
# Retruns TRUE
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{z logarithm}
\keyword{z transformation}
\keyword{zinvlog}
