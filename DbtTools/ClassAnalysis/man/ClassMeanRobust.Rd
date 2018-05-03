\name{ClassMeanRobust}
\alias{ClassMeanRobust}
\title{ Class Median Robust}
\description{
Calculates the mean of each group of data using a robust algorithm.}
\usage{
ClassMeanRobust(Data, Cls, LEAVEOUTPERCENTAGE)
}
\arguments{
  \item{Data}{ A matrix containing Data. Cases as rows, variables as columns. }
  \item{Cls}{ The clusterNumber of a given element of data.}
  \item{LEAVEOUTPERCENTAGE}{ Percentage of data that is left out. Default:  LEAVEOUTPERCENTAGE = 20}
}
% \details{}
\value{
\item{uniqueClasses}{The unique classes in cls. }
\item{meanPerClass}{The mean of the data points of an element of uniqueClasses. }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Michael Thrun}
% \note{ ~~further notes~~ }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
% \examples{}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
