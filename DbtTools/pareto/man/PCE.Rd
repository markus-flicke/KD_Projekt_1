\name{PCE}
\alias{PCE}
\title{Pareto Density Estimation}
\description{
This function  performs numerically integration of the Pareto Density Estimation using trapezoid method.}
\usage{
PCE(data,paretoRadius=NaN)
}
\arguments{
  \item{data}{numeric vector with data.}
  \item{paretoRadius}{numeric value, the Pareto Radius. If missing it is calculated.}
}

\value{
  \item{kernels }{numeric vector. data values at which Pareto Density is measured. }
  \item{integralParetoDensity }{numeric vector with densities calculated by Pareto Radius.}
  \item{paretoRadius }{numeric value, the Pareto Radius.}
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}
