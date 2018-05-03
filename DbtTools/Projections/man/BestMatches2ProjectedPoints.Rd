\name{BestMatches2ProjectedPoints}
\alias{BestMatches2ProjectedPoints}

\title{
BestMatches2ProjectedPoints
}
\description{
Converts quadgrid coordinates (= BestMatchingUnit coordinates) to  a set of cartesian (x,y) coordinates
}
\usage{
BestMatches2ProjectedPoints(Bestmatches, Lines)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Bestmatches}{
see package Umatrix
}
  \item{Lines}{
Optional: smaller Dimensions of overlying grid, default Lines= 50;
}
}
\value{
cartesian x,y coordinates
}
\author{
Michael Thrun
}