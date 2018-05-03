\name{ClassPrctile}
\alias{ClassPrctile}
\title{ Class Percentile}
\description{
Calculates the percentiles in each class of the data.}
\usage{
ClassPrctile(Data, Cls, P)
}
\arguments{
  \item{Data}{ A matrix containing the data. Cases as rows, variables as columns.}
  \item{Cls}{ A vector of class identifiers.}
  \item{P}{ A skalar for the percent value. }
}
% \details{}
\value{
\item{UniqueClasses}{The unique classes in cls. }
\item{Percentile}{The p-th percentile of the data points of a given unique class. }
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
