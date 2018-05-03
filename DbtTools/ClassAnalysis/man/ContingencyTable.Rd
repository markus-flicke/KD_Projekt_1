\name{ContingencyTable}
\alias{ContingencyTable}
\title{ Contingency Table}
\description{
 comfortable crosstabulation of instances in two different classifications
 uses MATLAB's function crosstab
}
\usage{
ContingencyTable(RowCls, ColCls)
}
\arguments{
  \item{RowCls}{A vector of class identifiers. }
  \item{ColCls}{ Another vector of class identifiers.}
}
% \details{}
\value{
\item{ctab}{ctab(i,j) contains the count of all Instances where
the i-th class in ACls equals the j- class in BCls }
\item{rowID}{The different classes in aCls, corresponding to the rows in ctab. }
\item{colID}{The different classes in aCls, corresponding to the columns of ctab. }
\item{aClassCount}{ Count of class instances according to aCls}
\item{bClassCount}{ Count of class instances according to bCls. }
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
