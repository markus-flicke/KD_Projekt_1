\name{inPSphere}
\alias{inPSphere}

\title{Data points in Pareto Sphere}
\description{
 This function determines the data points inside a ParetoSphere with ParetoRadius.
}
\usage{
inPSphere(data,paretoRadius=NULL)
}

\arguments{
  \item{data}{numeric matrix of data.}
  \item{paretoRadius}{numeric value. radius of P-spheres. If not given, calculate by the function 'paretoRad' }
}

\value{
  numeric vector with the number of data points inside a P-sphere with ParetoRadius.
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}

\keyword{ParetoRadius}

