\name{GroupPrctile}
\alias{ GroupPrctile}
\title{ Grup Percentiles }
\description{
Calculates the percentiles in each group of the data.}
\usage{
GroupPrctile(data, cls, p)
}
\arguments{
  \item{data}{ A matrix containing the data. Cases as rows, variables as columns. }
  \item{cls}{ A vector of class identifiers. }
  \item{p}{ Size of the percentile. }
}
% \details{}
\value{
\item{uniqueClasses}{ The unique classes in cls. }
\item{percentiles}{The p-th percentile of a geiven element of uniqueClasses. }
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
