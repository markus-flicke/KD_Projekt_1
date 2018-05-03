\name{MinInterClassDistance}
\alias{MinInterClassDistance}
\title{ Minimum Inter Class Distance }
\description{
the minimal euclidian distance of any of two points in two different classes
}
\usage{
MinInterClassDistance(data, cls)
}
\arguments{
  \item{data}{ A matrix containing data. Cases as rows, variables as columns. }
  \item{cls}{ A vector of class identifiers. }
}
% \details{}
\value{
\item{minInterDist}{ # \% the minimal euclidian distance of any of two points in two different classes}
\item{shortestInterDistances}{The shortest inter distance between two classes in cls. }
}
% \references{ ~put references to the literature/web site here ~ }
\author{ Raphael Paebst }
% \note{ ~~further notes~~ }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
% \examples{}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
