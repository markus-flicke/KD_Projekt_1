\name{sigmoid}
\alias{sigmoid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sigmoidplot}
\description{
  Plots the values \code{x} (in general a sigmoid) and fits a straigt to
  the slope.
}
\usage{
sigmoid(x, m = 0, t = 1, plotIt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The values for the sigmoid.}
  \item{m}{Adjust the bias of the values.}
  \item{t}{Adjust the slope of the straight.}
  \item{plotIt}{Do you want to see the plot?}
}
\details{
  In general m an t are values (e.g. 0.5). But you can also define m and
  t as 2 dimensional vectors m = c(x,y), t = c(a,b). The function \code{sigmoid}
  will calculate the values m and t based on the vectors.
}
\value{
  Returns a vector y with the values sigmoid(x). These are the y-values of
  f(x).
}
%\references{ ~put references to the literature/web site here ~ }
\author{Sascha Feld}
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
# Creae data.
x <- seq(-1,1,0.05)
sigmoid(x)
# Returns the y-values of the sigmoid function.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{sigmoid}% __ONLY ONE__ keyword per line