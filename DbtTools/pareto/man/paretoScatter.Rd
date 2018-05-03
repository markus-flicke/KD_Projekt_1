\name{paretoScatter}
\alias{paretoScatter}

\title{ Pareto Scatterplot}
\description{
  This function plots the Pareto densities.
}
\usage{
paretoScatter(x,y,plotIt=TRUE,binsXY=c(20,20))
}

\arguments{
  \item{x}{numeric vector. x data to be plotted.}
  \item{y}{numeric vector. y data to be plotted.}
  \item{plotIt}{logical, if plotting. TRUE by default.}
  \item{binsXY}{numeric vector with length 2. number of bins to use. 20,20 by default.}
}


\value{
  \item{nInBins}{the number of datapoints in ad bin centered at (gridX,gridY).}
  \item{gridX}{x coordinates of the bin center.}
  \item{gridY}{y coordinates if the bin center.}
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}
\note{Data may not contain missing values.
}

\keyword{Scatterplot}