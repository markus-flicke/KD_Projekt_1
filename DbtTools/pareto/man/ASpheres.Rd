\name{ASpheres}
\alias{ASpheres}
\title{A-Spheres}
\description{
  Computes the Pareto- values and densities.
}
\usage{
ASpheres(data,defined=NULL, attribute =NULL)
}
\arguments{
  \item{data}{numeric matrix of data. data in row, variables in column }
  \item{defined}{logical vector with length of data-column (variables). For variable = FALSE, this variable will not contribute the distances. TRUE by default.}
  \item{attribute}{logical vector with length of data-row (data). For attribute = TRUE, this attribute will contribute the value nWithAttribute. TRUE by default.}
}

\value{
  \item{radiusA}{Pareto-Radius of the A-Sphere.}
  \item{minParetoDensity }{This density is the minimum for a Pareto-Sphere.}
  \item{nInSphere}{Number of points, which are located in the respective Sphere.}
  \item{nWithAttribute}{Number of points with attribute, which are located in the respective Sphere.}
}
\author{Friederike Matz}
