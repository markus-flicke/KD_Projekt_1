\name{hypersphereSets}
\alias{hypersphereSets}
\title{Hypersphere Sets}
\description{
This function calculates the number of points inside a hypersphere around each data points
}
\usage{
hypersphereSets(data, hypersphereRadius)
}
\arguments{
  \item{data}{Data matrix with vectors in rows}
  \item{hypersphereRadius}{Radius of Hyperspheres}
}

\value{
 
  \item{nrPointsInSphere }{Number of data points in sphere around each point}
  \item{pSets }{logical matrix. pSets(i,j) ==TRUE <=> Euclidian distance(data[i,],data[j,]) < hypersphereRadius }
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}
