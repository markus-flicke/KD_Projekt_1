\name{NormalizeCls}
\alias{NormalizeCls}
\title{ Normalize Cls }
\description{
Class identifiers are recoded as positive consecutive integers.}
\usage{
NormalizeCls(Cls)
}
\arguments{
  \item{Cls}{ A vector of class identifiers. }
}
% \details{}
\value{
\item{ normalizedCls}{Cls consistently recoded to positive consecutive integers}
\item{normalizedClasses}{The different class numbers in normalizedCls}
\item{ uniqueCls}{The different class numbers in Cls such that normalizedCls(i) <-> UniqueCls(i)}
\item{a nzClasses}{The number of different classes}
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
