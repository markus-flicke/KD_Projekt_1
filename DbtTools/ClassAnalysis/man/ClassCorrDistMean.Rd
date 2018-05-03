\name{ClassCorrDistMean}
\alias{ClassCorrDistMean}
\title{ Class Correlation Mean}
\description{
Calculates the mean of correlation distances in each class.}
\usage{
ClassCorrDistMean(data, cls)
}
\arguments{
  \item{data}{ Matrix containing the data. Cases in rows, variables in columns. }
  \item{cls}{ cls(i) is the clusterNumber of a given data(i)}
}
% \details{}
\value{
\item{uniqueClasses}{The unique Classes in cls}
\item{meanCorrDistPerClass}{The mean correlation distance for each class in uniqueClass }
}
% \references{ ~put references to the literature/web site here ~ }
\author{ Raphael Paebst}
% \note{ ~~further notes~~ }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
% \examples{}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
