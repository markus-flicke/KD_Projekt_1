\name{ClassStd}
\alias{ClassStd}
\title{ Class Standard Deviation }
\description{
Calculates the standard deviation in each group of data.
}
\usage{
ClassStd(Data, Cls)
}
\arguments{
  \item{Data}{ A matrix containing the data. Cases as rows, variables as columns.}
  \item{Cls}{ A vector containing the class identifiers.}
}
% \details{}
\value{
\item{uniqueClasses}{The unique classes in cls. }
\item{stdPerClass}{The standard deviation of the data points in uniqueClasses.}
}
% \references{ ~put references to the literature/web site here ~ }
\author{ Raphael Paebst}
% \note{ ~~further notes~~  }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
% \examples{}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
