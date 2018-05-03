\name{PDEstimationForGauss}
\alias{PDEstimationForGauss}

\title{PDE Estimation For Gauss}
\description{
This function calculates the ParetocDensity for an one dimensional distribution.}
\usage{
PDEstimationForGauss(data,paretoRadius=0,kernels=NaN)
}

\arguments{
  \item{data}{numeric vector of data.}
  \item{paretoRadius}{numeric, the Pareto Radius. If omitted, it will be calculated.}
  \item{kernels}{numeric vector, data values at which ParetoDensity is measured.}
}

\value{
  \item{kernels }{numeric vector. data values  Pareto Density is measured at.}
  \item{paretoDensity}{numeric vector containing the determined density by ParetoRadius.}
  \item{paretoRadius}{numeric value.}
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}
