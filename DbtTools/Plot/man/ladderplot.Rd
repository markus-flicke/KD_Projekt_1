\name{ladderplot}
\alias{ladderplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Ladderplot }
\description{
  Plots the columns of the data values in a ladder diagram
}
\usage{
ladderplot(Data, Names, cls, filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{ Data to be plotted. data(d,n) d - number of data entries, n - number of variables }
  \item{Names}{ Optional: Names of the columns as captions of the ladders if the dimension != d }
  \item{cls}{ Optional: Integers as type encoding: will be converted in colours. 0 = no classification. 6 colours will be repeated  }
  \item{filename}{ Optional: filename.eps - file for the plot to be saved in }
}
\references{  }
\author{databionics UnivMarburg}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ladderplot }
%\keyword{ ladder }
%\keyword{ plot }