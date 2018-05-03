\name{errorbarh}
\alias{errorbarh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Errorbar horizontal }
\description{
  Plots a horizointal +- errorbar graph with given Low and High bounds
}
\usage{
errorbarh(MeanData, LowBound, HighBound, Labels, MeanColor, LineColor)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MeanData}{ means to be plotted }
  \item{LowBound}{ wiskers are [MeanData-LowBound, MeanData+HighBound] }
  \item{HighBound}{ Optional: wiskers are [MeanData-LowBound, MeanData+HighBound] if not given: HighBound=LowBound }
  \item{Labels}{ Optional: y axis tick labels (array of char), or ==0 of not wanted }
  \item{MeanColor}{ Optional: plot symnol as mark for mean 'b*' by default }
  \item{LineColor}{ Optional: color of the bars, 'b' == blue by default }
}
\value{
  \item{Plothandle}{Plot Handle as return by function plot from dbt.Plot package}
}
\author{databionics UnivMarburg}
\seealso{ \code{\link{plot}} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ error }
\keyword{ bar }
\keyword{ errorbar }
\keyword{ plot }
