\name{SignificantContingencyTable}
\alias{SignificantContingencyTable}
\title{ SignificantContingencyTable }
\description{
see if actual clusters are significant differnt from expected 
uses [Lower, Upper] = binomialQuantiles(ns, p, alpha);
}
\usage{
SignificantContingencyTable(Xtab, ErrorNiveau)
}
\arguments{
  \item{Xtab}{ A crosstabulation produced for example by contingencyTable. }
  \item{ErrorNiveau}{ The error niveau. }
}
% \details{} 
\value{
\item{ significantRelDiff}{Relative Differences, that are Significant. }
\item{ expectedXtab}{ The Xtab of expected values}
\item{ isSignificant}{ The given number of members of class i differs significantly.}
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
