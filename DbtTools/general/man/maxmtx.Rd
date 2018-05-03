\name{maxmtx}
\alias{maxmtx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Maximum value and its positon of a matrix}
\description{
  maxmtx returns the maximum value and its positon of a matrix.
}
\usage{
maxmtx(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{x} is a matrix.}
}
\details{
  maxmtx will search column by column.
}
\value{
  Returns the a vector with the max value of \code{x} and the
  position (row/column) of the first appearance.
  Vector:
  \item{max}{The max value.}
  \item{row}{The row.}
  \item{col}{The column.}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Sascha Feld}
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
# Create Data.
x <- matrix(c(1:9),3,3)
maxmtx(x)
# Returns the vector: max row col
#                       9   3   3
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{maxmtx}
\keyword{maximum value}