\name{PCEplot}
\alias{PCEplot}

\title{ PCE Plot}
\description{
 This function plots the numerically integration of the Pareto Density Estimation.
}
\usage{
PCEplot(data,paretoRadius=0,plot=TRUE,title='')
}

\arguments{
  \item{data}{numeric vector with data.}
  \item{paretoRadius}{numeric value, the Pareto Radius. If missing it will be calculated.}
  \item{plot}{logical, if plot. TRUE by default.}
  \item{title}{character vector. Title of the plot. Optional.}
}

\value{
   \item{kernels }{numeric vector. data values at which Pareto Density is measured. }
  \item{integralParetoDensity }{numeric vector with densities calculated by Pareto Radius.}
  \item{paretoRadius }{numeric value, the Pareto Radius.}
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}

\keyword{Pareto}
\keyword{Plot}
