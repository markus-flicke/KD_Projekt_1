\name{uniquePoints}
\alias{uniquePoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Detect multiple appearances}
\description{
  Erase all multiple appearances of Points.
}
\usage{
uniquePoints(data,mindist= 1e-10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The vector/matrix with data points. If it's a matrix, the
  points should be in rows.}  
	\item{mindist}{everything with less distance is considered equal}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  Returns a list:
  \item{unique}{The matrix with no multiple appearance.}
  \item{sorted}{The "unique" indices (first appearance).}
  \item{mergeind}{Index, which includes multiple appearance of indices. Can be
  used to recover the original data matrix.
}
}
%\references{ ~put references to the literature/web site here ~ }
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
# Create data.
x <- c(1,1,7,8,2,2,9,3,3,3,9,7)
x <- matrix(x,4,3)
uniquePoints(x)
# Returns a matrix: 1 2 3
#                   7 9 9
#                   8 3 7
# a vector: 1,3,4 (unique indices)
# and a vector: 1,1,3,4 (multiple appearance).
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{unique points}
\keyword{uniquePoints}
\keyword{multiple appearance}