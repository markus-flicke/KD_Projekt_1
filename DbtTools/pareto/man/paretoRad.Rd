\name{paretoRad}
\alias{paretoRad}
\title{ParetoRadius}
\description{
  This function calculates the ParetoRadius.
}
\usage{
paretoRad(data,maximumNrSamples=1000,expectedNumberOfClusters=6,plotDistancePercentiles=FALSE,verbose=FALSE)
}
\arguments{
  \item{data}{numeric matrix of data, cases in rows, variables in columns.}
  \item{maximumNrSamples}{numeric. Maximum number for which the distance calculation can be done. 1000 by default.}
  \item{expectedNumberOfClusters}{number of clusters that are expected in the data set. 6 by default. }
  \item{plotDistancePercentiles}{logical. If TRUE, a plot of the percentiles of distances is produced. FALSE by default.}
  \item{verbose}{logical. If TRUE, print massage what the function is doing. FALSE by default.}
}
\details{
paretoRad uses  paretoRadiusfuerGMM for 1D and paretoRadMultivatiate for higher dimensions, such that median of paretoSpheres contains an information optimal number of data points (20.13%) modified by the expected number of clusters.
Dee [Ultsch03] A. Ultsch:"Optimal density estimation in data containing clusters of unknown structure" for details.
}
\value{
 numeric value, the Pareto radius.
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}
\note{ 
In the case of univariate data the radius is determined using 
paretoRadiusfuerGMM; if this is unwanted, use  ParetoRadMultivatiate instead.
}
