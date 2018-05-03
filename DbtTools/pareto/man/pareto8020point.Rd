\name{pareto8020point}
\alias{pareto8020point}
\title{Pareto 80 20 point}
\description{
  This function finds the Pareto point for given PercCumCost, PercentCumYield
}
\usage{
pareto8020point(percentCumCost, percentCumYield, plotIt=FALSE)
}
\arguments{
  \item{percentCumCost}{numeric vector. define the Pareto Plot ( sorted! and in [0 1] range}
  \item{percentCumYield}{numeric vector. define the Pareto Plot ( sorted! and in [0 1] range}
  \item{plotIt}{logical, if a plot is made. FALSE by default.}
}

\value{
paretoInd, numeric value. PercCumCost(ParetoInd),PercentCumYield(ParetoInd) is the Pareto Point i.e the point closest to (0,1)

}
\references{Matlab dbt - Pareto}
\author{Friederike Matz}

\keyword{pareto }
