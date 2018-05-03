\name{paretoClasses}
\alias{paretoClasses}
\title{Pareto classes}
\description{
  This function calculates the ABCD classes for PDE and estimates the ParetoRadius.
}
\usage{
paretoClasses(data,verbose=FALSE,paretoRadius=0)
}
\arguments{
  \item{data}{numeric matrix with data. cases in rows, variables in columns.}
  \item{verbose}{logical. If TRUE : print massages what the function is doing. FALSE by default.}
  \item{paretoRadius}{numeric value. If not giving, the ParetoRadius is calculated using 'paretoRad'}
}

\value{
  \item{abcdCls }{numeric vector containing the density classification of data: A-cls = 1, B-cls = 2, C-cls = 3, D-cls = 4.}
  \item{paretoRadius }{numeric vector. the paretoRadius.}
  \item{nInPspheres }{numeric vector. numnber of points in ParetoSphere around each data point.}
  \item{densityLimits }{numeric vector with density limits for each class.}
  
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}
