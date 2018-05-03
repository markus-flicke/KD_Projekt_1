\name{ClassTrimMean}
\alias{ClassTrimMean}
\title{ Class Trim Mean}
\description{
Excludes the highest and lowest percent/2 from the calculation of the mean. }
\usage{
ClassTrimMean(data, cls, percent)
}
\arguments{
  \item{data}{ A matrix containing the data. Cases as rows, variables as columns.}
  \item{cls}{ A vector containing class identifiers.}
  \item{percent}{The highest and lowest percent to be excluded from the calculations. 10 by default.}
}
% \details{}
\value{
\item{uniqueClasses}{The unique classes in cls.}
\item{trimMeanPerClass}{The trimmed mean of the data points in a given element of uniqueClasses.}
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
