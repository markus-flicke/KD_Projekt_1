\name{eqfloat}
\alias{eqfloat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compares 2 floating point numbers}
\description{
  Compares 2 floating point numbers up to precision \code{precision}
  after the digit.
}
\usage{
eqfloat(f, g, precision = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f,g}{\code{f,g} are the floating point numbers, which will be compared up to}
  \item{precision}{Precision \code{precision} (decimal place). Precision has to be >=0.}
}
%\details{
%}
\value{
  Returns TRUE if the floating point numbers are equal, defined by \code{precision}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Sascha Feld}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
# Create 2 floating point numbers.
x <- 1.23378
y <- 1.23367
eqfloat(x,y,3)
# Returns TRUE
eqfloat(x,y)
# Retruns FALSE
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{compare floating point number}
\keyword{compare}
\keyword{eqfloat}
