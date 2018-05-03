\name{sortby}
\alias{sortby}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sort a matrix by column}
\description{
  Sorting a matrix by column \code{column}.
}
\usage{
sortby(m, column, desc = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{\code{m} is the Matrix, which will be sorted}
  \item{column}{\code{column} defines at which column the matrix should be sorted}
  \item{desc}{Sorting descending?}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  Returns a sorted matrix.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Sascha Feld}
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{sortdescending}}}
\examples{
# Create data.
a <- t(matrix(c(9,9,9,8,1,8,7,3,7,6,6,6),3,4))
sortby(a,2)
# Returns a sorted matrix by column 2.
#     8     1     8
#     7     3     7
#     6     6     6
#     9     9     9
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{sortby}
\keyword{sorting by column}
