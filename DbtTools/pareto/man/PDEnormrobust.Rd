\name{PDEnormrobust}
\alias{PDEnormrobust}

\title{PDE norm robust}
\description{
This function plots ParetoDensityEsrtimation (PDE) and Gaussian with empirical Mean and  Variance, using a robust measure for variance.}
\usage{
PDEnormrobust(data,paretoRadius=0,plotIt=TRUE)
}

\arguments{
  \item{data}{numeric vector. data to be plotted.}
  \item{paretoRadius}{numeric, the Pareto Radius. If omitted, calculate by function paretoRad.}
  \item{plotIt}{logical, if plot. TRUE by default.}
}

\value{
 \item{kernels }{numeric vector. The x points of the PDE function.}
  \item{paretoDensity }{numeric vector,  the PDE(x).}
  \item{paretoRadius }{numeric value, the Pareto Radius used for the plot.}
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}
