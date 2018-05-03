\name{PDEgmm}
\alias{PDEgmm}
\title{PDE with gaussian mixtures}
\description{
This function plots PDE with gaussian mixtures and bins.}
\usage{
PDEgmm(kernels,paretoDensity,means,stds=NaN,alphas=NaN,bins=NULL,bw=FALSE)
}
\arguments{
  \item{kernels}{numeric vector, kernels from PDE.}
  \item{paretoDensity}{numeric vector, density values at kernels.}
  \item{means}{numeric vector, means of mixtures.}
  \item{stds}{numeric vector, studs of mixtures. optional.}
  \item{alphas}{numeric vector, weights of mixtures. optional.}
  \item{bins}{numeric vector of bin boundaries.}
  \item{bw}{logical, if black and white plots. FALSE by default.}
}

\references{Matlab dbt - Pareto}
\author{Friederike Matz}