\name{paretoDensity1D}
\alias{paretoDensity1D}

\title{ Pareto Density 1D}
\description{
	 This function calculates the ParetoDensity for a one dimensional distribution.
}
\usage{
paretoDensity1D(data,paretoRadius=0)
}
\arguments{
  \item{data}{numeric vector of data. }
  \item{paretoRadius}{numeric value, the ParetoRadius. OPTIONAL}
}
\details{
}
\value{
  \item{kernels }{numeric vector. data values at which ParetoDensity is measured.}
  \item{paretoDensity }{numeric vector of the estimated density.}
  \item{paretoRadius}{numeric value, the ParetoRadius.}
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}
