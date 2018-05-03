\name{rsample}
\alias{rsample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Random indices}
\description{
  Creates a random vector of indices.
}
\usage{
rsample(k, n, uniq = TRUE, exact = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{The length of the vector is given by \code{k}.}
  \item{n}{\code{n} defines the maximum of an index.}
  \item{uniq}{Should any generated index be unique?}
  \item{exact}{Use this with the \code{uniq} parameter. Be sure to get \code{k} values, if there were duplicates.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  Returns a vector, which has the length \code{k} and the maxindex \code{n}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Sascha Feld}
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
rsample(10,10)
# Returns a vector of length 10 and max index 10.
rsample(10,10,exact=FALSE)
# Returns a vector of length 10 with max index 10. Every Index is NOT unique.
# So if there were duplicates you should get a vector with lenght < 10.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{rsample}
\keyword{random vector}
\keyword{random indices}
