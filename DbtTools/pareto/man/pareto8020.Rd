\name{pareto8020}
\alias{pareto8020}
\title{Pareto 80 20}
\description{
  This function finds the best point for minimal number of elements to get maximal yield of a data vector. 
}
\usage{
pareto8020(yield,plotIt=FALSE)
}
\arguments{
  \item{yield}{numeric vector of data. }
  \item{plotIt}{logical, if a plot is made. FALSE by default.}
}

\value{

  \item{optimalInd}{numeric vector. yeald[OptimalInd] is sorted descending and sum(yeald[OptimalInd]) is maximal for minimal(length(OptimalInd)) }
  \item{paretoInd}{numeric. The 80/20 point of PercCumCost(ParetoInd),PercentCumYeald(ParetoInd) }
  \item{percCumCost}{numeric vector. Cumulative costs in percent.}
  \item{percentCumYeald}{numeric vector. Cumulative yield in percent.}
 
}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}
\note{
The data may not contain missing values.
	
}
