\name{sortdescending}
\alias{sortdescending}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sort in descending order}
\description{
  Sorts a vector/matrix \code{x} in descending order.
}
\usage{
sortdescending(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Vector or Matrix \code{x}.}
}
\details{
  If \code{x} is a matrix, it's sorted column by column. (see examples)
}
\value{
  Returns a list, where the first element is the sorted vector/matrix and
  the second element is a vector/matrix with the sorted indices.
  \item{sort}{Sorted items.}
  \item{indices}{Vector/Matrix of sorted indices.}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Sascha Feld}
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{sortby}}}
\examples{
# Create data.
x <- c(1,9,9,27,4,8)
y <- matrix(c(1,15,34,9,9,1,2,9,7,1,3,9),4,3)
sortdescending(x)
# Retruns a list,
# $sort
# 27,9,9,8,4,1
# $indices
# 4,2,3,6,5,1
sortdescending(y)
# Retruns a list
# $sort
#      [,1] [,2] [,3]
# [1,]   34    9    9
# [2,]   15    9    7
# [3,]    9    2    3
# [4,]    1    1    1
#
# $indices
#     [,1] [,2] [,3]
# [1,]    3    1    4
# [2,]    2    4    1
# [3,]    4    3    3
# [4,]    1    2    2

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{sortdescending}
\keyword{descending order}
