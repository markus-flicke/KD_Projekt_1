\name{triuvec}
\alias{triuvec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Upper triangular matrix}
\description{
  Returns a vector of the upper triangular matrix.
}
\usage{
triuvec(matrix, k = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix}{A matrix.}
  \item{k}{At which diagonal do you wish to start?}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A vector, which includes the upper triangular matrix.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Sascha Feld}
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{trilvec}}}
\examples{
# Create data.
x <- matrix(c(1:9),3,3)
trilvec(x)
# Returns a vector = 1,4,5,7,8,9
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{triuvec}
\keyword{upper triangular matrix}
