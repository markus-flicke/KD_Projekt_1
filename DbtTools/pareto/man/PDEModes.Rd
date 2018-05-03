\name{PDEModes}
\alias{PDEModes}

\title{PDE Modes}
\description{
This function finds mode(s) of PDE - So far only one is searched.}
\usage{
PDEModes(data)
}

\arguments{
  \item{data}{numeric vector of data.}
}

\value{
  \item{modes }{x value(s) of mode(s)}
  \item{heights }{y value(s) of mode(s)}
  \item{areas}{percentiles in mode bins}
  \item{kernels }{numeric vector of kernels returned from PDEstimationForGauss.}
  \item{pde}{numeric vector of Pareto density returned from PDEstimationForGauss.}
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}

