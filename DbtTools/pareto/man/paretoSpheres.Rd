\name{paretoSpheres}
\alias{paretoSpheres}

\title{Pareto Spheres}
\description{
  This function calculates Pareto Spheres around each row in data.
}
\usage{
paretoSpheres(data, defined=NaN)
}

\arguments{
  \item{data}{numeric datamatrix with vectors in rows.}
  \item{defined}{logical vector. TRUE for columns to be used. If missing, all columns are used.}
}

\value{
  \item{nn }{logical matrix, if data is in Pareto Sphere}
  \item{mc }{numeric vector. number of points in Pareto Sphere for each row.}
  \item{pr }{numeric value, the Pareto Radius.}
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}

\keyword{Pareto}
