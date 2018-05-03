\name{PDEscatterApprox}
\alias{PDEscatterApprox}
\title{PDE scatter approximation}
\description{
This function plots the PDE on top of a scatterplot (fast approximation) }
\usage{
PDEscatterApprox(x,y,paretoRadius=0,drawTopView=TRUE,nrOfContourLines=20,dataPointLineSpec='b',nInPspheres=NaN)
}
\arguments{
  \item{x}{numeric vector, data in x dimension.}
  \item{y}{numeric vector, data in y dimension.}
  \item{paretoRadius}{numeric, the Pareto Radius. If omitted, calculate by paretoRad.}
  \item{drawTopView}{logical. If TRUE contour is drawn (default), otherwise a 3D plot is drawn. }
  \item{nrOfContourLines}{numeric, number of contour lines to be drawn. 20 by default.}
  \item{dataPointLineSpec}{ lineSpec of data points.}
  \item{nInPspheres}{numeric vector. If the number of points inside Pareto Spheres is known this is used as z values for the plot, otherwise it will be calculated.}
}

\value{
 numeric, the number of Points inside the Pareto Sphere around each point.
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}
