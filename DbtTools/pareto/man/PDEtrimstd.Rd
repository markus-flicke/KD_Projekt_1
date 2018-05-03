\name{PDEtrimstd}
\alias{PDEtrimstd}

\title{PDE trimmed std}
\description{
std trimmed to the most dense regions in data. The top 100-TrimPercentile from PDEstimationForGauss(data) are used to calculate the std.}
\usage{
PDEtrimstd(data, trimPercentile)
}

\arguments{
  \item{data}{numeric matrix of data. }
  \item{trimPercentile}{numeric vector of percentile.}
}

\value{
  trimmedSdev
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}
