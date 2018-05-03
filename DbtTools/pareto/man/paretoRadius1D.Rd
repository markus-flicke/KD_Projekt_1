\name{paretoRadius1D}
\alias{paretoRadius1D}

\title{Pareto Radius for 1D data}
\description{
Fast Pareto Radius estimation, optimal for 1D Gaussian mixtures: 18th percentile of pairwise Euclidian distances.}
\usage{
paretoRadius1D(data,maximumNrSamples=1000,plotDistancePercentiles=FALSE)
}

\arguments{
  \item{data}{numeric datamatrix. cases in rows, variables in columns.}
  \item{maximumNrSamples}{ numeric. Maximum number for which the distance calculation can be done. 1000 by default.}
  \item{plotDistancePercentiles}{logical. If TRUE a plot of the percentiles of distances is produced. FALSE by default.}
}

\value{
 numeric value, the Pareto Radius.
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}

\keyword{Pareto Radius} 
