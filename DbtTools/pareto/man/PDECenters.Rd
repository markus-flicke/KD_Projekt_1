\name{PDECenters}
\alias{PDECenters}
\title{PDE Centers}
\description{
This function calculates of most dense  points in a data set.
Makes use of the Pareto Density Estimation}
\usage{
PDECenters(data, paretoRadius=NaN)
}

\arguments{
  \item{data}{data, each case in a row.}
  \item{paretoRadius}{the Parato Radius, if omitted it will be calculated by ParetoRad.}
}

\value{
  \item{centersInd }{a vector of indices with the most dense centers in data.}
  \item{nInSphere }{PDE in every row of data.}
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}
