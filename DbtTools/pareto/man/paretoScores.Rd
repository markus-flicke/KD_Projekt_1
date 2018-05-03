\name{paretoScores}
\alias{paretoScores}

\title{ Pareto Scores}
\description{
This function calulates Pareto score (see paretoScore) for
all features and all classes.
}
\usage{
paretoScores(data, cls)
}

\arguments{
  \item{data}{ numeric matrix of data, features in columns. }
  \item{cls}{numeric class vector.}
}
\details{
}
\value{
  \item{p}{pareto score}
  \item{b }{Bayes error}
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}