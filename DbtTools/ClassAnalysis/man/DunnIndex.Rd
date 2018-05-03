\name{DunnIndex}
\alias{DunnIndex}
\title{Dunn Index }
\description{
Calculates the Dunn Index as a measure of the clustering quality. }
\usage{
DunnIndex(data, cls)
}
\arguments{
  \item{data}{A matrix, containing data. Cases as rows, variables as columns.  }
  \item{cls}{ A vector containing class identifiers. }
}
% \details{}
\value{
\item{dunn}{ The Dunn index}
\item{maxInnerDist}{The maximum inner class distance}
\item{minInterDist}{The minimum inter class distance }
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
