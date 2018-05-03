\name{inPSphereAtKernels}
\alias{inPSphereAtKernels}
\title{ In P-sphere at kernels}
\description{
  This function determines for given points (Kernels) the data points inside a ParetoSphere with ParetoRadius.
}
\usage{
inPSphereAtKernels(kernels,paretoRadius=NULL,data=NULL,kernels2DataDistance=NULL)
}
\arguments{
  \item{kernels}{numeric matrix, giving the centers at which spheres are considered. }
  \item{paretoRadius}{numeric value. radius of P-spheres. If not given, calculate by the function 'paretoRad'.}
  \item{data}{numeric matrix with data. If not giving, data will be equal to kernels.}
  \item{kernels2DataDistance}{numeric vector containing all distances of kernels to data. optinal}
}

\value{
  \item{isInSphere }{binary vector: isInSphere[i]=1 means data[i] is in (at least) one P-sphere.}
  \item{inSphereInd }{numeric vector. giving the index of those data points in at least one P-sphere.}
  \item{nInPspheres }{numeric vector. giving the number of data points inside a P-sphere with ParetoRadius around Kernels.}
  \item{kernels2DataDistance }{numeric vector containing all distances of kernels to data. }
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}

\keyword{ParetoRadius}

