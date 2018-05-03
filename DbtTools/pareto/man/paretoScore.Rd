\name{paretoScore}
\alias{paretoScore}

\title{ Pareto Score}
\description{
This function calculates classification error made when segmenting the range into intervals at the Bayesian decision boundaries, i.e. all crossings of two probability density functions and using the denser class in each interval.
Both densities are estimated with the Pareto Density Estimation unless a faster, suboptimal method is requested.
}
\usage{
paretoScore(x,y,fast=FALSE)
}

\arguments{
  \item{x}{numeric vector of data. }
  \item{y}{ numeric vector of data.}
  \item{fast}{logical, if fast version  without paretoradius is used. FALSE by default.}
}

\value{
  \item{p}{pareto score}
  \item{b }{Bayes error}
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}

