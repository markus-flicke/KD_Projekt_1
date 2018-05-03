\name{trilvec}
\alias{trilvec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Lower triangular matrix}
\description{
  Returns a vector of the lower triangular matrix.
}
\usage{
trilvec(matrix, k = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix}{A matrix.}
  \item{k}{At which diagonal do you wish to start? The default value
  includes the main diagonal.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A vector, which includes the lower triangular matrix.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Sascha Feld}
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{triuvec}}}
\examples{
# Create data.
x <- matrix(c(1:9),3,3)
trilvec(x)
# Returns a vector = 1,2,3,5,6,9
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{trilvec}
\keyword{lower triangular matrix}
