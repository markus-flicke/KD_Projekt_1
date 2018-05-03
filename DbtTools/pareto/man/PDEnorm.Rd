\name{PDEnorm}
\alias{PDEnorm}

\title{PDE norm}
\description{
This functions plots ParetoDensityEsrtimation (PDE) and Gaussian with empirical Mean and  Variance}
\usage{
PDEnorm(data,paretoRadius=NaN, yl='default', main='default')
}

\arguments{
  \item{data}{numeric vector, data to be plotted.}
  \item{paretoRadius}{numeric value, the Pareto Radius. Optional, calculated if omitted. }
  \item{yl}{character vector. y label of the plot. Optional}
  \item{main}{character vector. y label of the plot. Optional}
}

\value{
  \item{kernels }{numeric vector. The x points of the PDE function.}
  \item{paretoDensity }{numeric vector,  the PDE(x).}
  \item{paretoRadius }{numeric value, the Pareto Radius used for the plot.}
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}
