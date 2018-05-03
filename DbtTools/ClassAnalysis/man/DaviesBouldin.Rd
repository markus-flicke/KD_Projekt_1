\name{DaviesBouldin}
\alias{DaviesBouldin}
\title{ Davies Bouldin}
\description{
Calculates the Davies Bouldin index as a measure of the clustering quality.}
\usage{
DaviesBouldin(data, cls)
}
\arguments{
  \item{data}{ A matrix containing data. Cases as rows, variables as columns.}
  \item{cls}{ A vector of class identifiers.}
}
% \details{}
\value{
\item{dbIndex}{The Davies Bouldin Index}
\item{centerPerClass}{The center of each cluster}
\item{meanClass2CenterDist}{ mean distances to the cluster center. }
\item{clusterCenterDist}{DistanceMatrix(meanPerClass) }
}
% \references{ ~put references to the literature/web site here ~ }
\author{ Raphael Paebst}
% \note{ ~~further notes~~ }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
% \examples{}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
