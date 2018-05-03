\name{ClsToTrueCls}
\alias{ClsToTrueCls}
\title{ Cls to True Cls }
\description{
 GivenCls is renamed such that Accuracy = sum(TrueCls==RenamedCls)/length(TrueCls) is maximal
 comfortable crosstabulation of instances in two different classifications
 uses MATLAB's function crosstab
}
\usage{
ClsToTrueCls(trueCls, givenCls)
}
\arguments{
  \item{trueCls}{ A vector of class identifiers.}
  \item{givenCls}{ Another vector of class identifiers. }
}
% \details{}
\value{
\item{accuracy}{sum(trueCls==renamedCls)/length(trueCls).}
\item{renamedCls}{A vector containing the renamed class identifiers.}
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
