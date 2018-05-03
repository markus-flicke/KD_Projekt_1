\name{CIndex}
\alias{CIndex}
\title{ C-Index}
\description{
Calculates the C-Index as a measure of clustering quality. The lower, the better.}
\usage{
CIndex(data, cls)
}
\arguments{
  \item{data}{ A matrix containing the data.}
  \item{cls}{ cls(i) is the cluster containing data(i)}
}
% \details{}
\value{
\item{cHuber}{The C-Index}
\item{innerDist}{Inner class distances}
\item{ sMin}{smallest paired distances}
\item{sMax}{ The largest paired distances } 
 }
% \references{ ~put references to the literature/web site here ~ }
\author{ Raphael Paebst}
 %\note{ ~~further notes~~ }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
% \examples{}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
