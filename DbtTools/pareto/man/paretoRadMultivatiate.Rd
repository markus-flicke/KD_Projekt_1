\name{paretoRadMultivatiate}
\alias{paretoRadMultivatiate}

\title{ParetoRadius Multivariate}
\description{
Calculation of the ParetoRadius such that median of paretoSpheres contains the 20.13*inner2Inter(expectedNumberOfClusters)- percentile of data points modyfied by the expectet number of clusters.
see [Ultsch03:  Optimal density estimation in data containing clusters of unknown structure] for details.
NOTE: better call ParetoRad  with the same parameters !!!!!}
\usage{
paretoRadMultivatiate(data,maximumNrSamples=1000,expectedNumberOfClusters=6,plotDistancePercentiles=FALSE,verbose=FALSE)
}

\arguments{
  \item{data}{numeric datamatrix, cases in rows, variables in columns.}
  \item{maximumNrSamples}{maximum number for which the distance calculation can be done. 1000 by default.}
  \item{expectedNumberOfClusters}{number of expected clusters in the data set. 6 by default.}
  \item{plotDistancePercentiles}{logical. If TRUE, a plot of the percentiles of distances is produced. FALSE by default.}
  \item{verbose}{logical. If TRUE, print massage what the function is doing. FALSE by default.}
}


\value{
  numeric value, the Pareto radius. 
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}

\keyword{Pareto}
\keyword{Radius}
