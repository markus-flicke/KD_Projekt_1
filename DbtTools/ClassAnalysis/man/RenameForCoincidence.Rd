\name{RenameForCoincidence}
\alias{RenameForCoincidence}
\title{ RenameForCoincidence }
\description{
Renames classes in a given classifications so it matches a standard classification }
\usage{
RenameForCoincidence(givenCls, standardCls)
}
\arguments{
  \item{givenCls}{ A vector of classes comprising a classification.}
  \item{standardCls}{ A vector of classes comprising a standard classification.}
}
% \details{}
\value{
\item{renamedCls}{The renamed cls with minimal differences to standardCls }
\item{bestAccuracy}{ The best accuracy obtained by renaimng. }
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
