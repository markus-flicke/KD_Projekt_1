\name{ClassMax}
\alias{ClassMax}
\title{ Class Max}
\description{
Calculates the maximum in each group of the data.}
\usage{
ClassMax(Data, Cls)
}
\arguments{
  \item{Data}{ A matrix containing data. Cases as rows, variables as columns.}
  \item{Cls}{ The clusterNumber of a given element of data.}
}
% \details{}
\value{
\item{uniqueClasses}{The unique classes in cls.}
\item{maxPerClass}{The maximum of data Points in a given element of uniqueClasses. }
}
% \references{ ~put references to the literature/web site here ~ }
\author{ Raphael Paebst}
% \note{ ~~further notes~~ }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
% \examples{ }}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
