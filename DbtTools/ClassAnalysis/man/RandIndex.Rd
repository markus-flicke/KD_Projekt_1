\name{RandIndex}
\alias{RandIndex}
\title{ Rand Index }
\description{
Calculates different indices.}
\usage{
RandIndex(aCls, bCls)
}
\arguments{
  \item{aCls}{A vector of class identifiers. }
  \item{bCls}{ Another vector of class identifiers. }

}
% \details{}
\value{
\item{ adjRandIndex}{Hubert & Arabie adjusted Rand index, 0 = random, 100\% = total agreement}
\item{ rawRandIndex}{unadjusted Rand index = p(agreement)}
\item{ mirkinIndex}{"Mirkin's" index = p(disagreement)}
\item{ hubertIndex}{"Hubert's" index = p(agree)-p(disagree)}
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
