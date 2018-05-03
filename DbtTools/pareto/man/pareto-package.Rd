\name{pareto-package}
\alias{pareto-package}
\alias{pareto}
\docType{package}
\title{
DataBionics Toolbox - Pareto
}
\description{
This package contains many functions to compute and plot the  Pareto radius and Pareto density.
}
\details{
\tabular{ll}{
Package: \tab dbt.pareto\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2016-08-12\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
The package is based on the pareto density estimation of the CRAN package AdaptGauss
}
\author{
Friederike Matz,Felix Pape, Michael Thrun
}
\references{
see AdaptGauss package
}

\keyword{ pareto }
\keyword{ radius}
\keyword{ density }
\keyword{PDE}
\keyword{PDEplot}

\examples{
data("LKWFahrzeitSeehafen2010")
PDEplot(LKWFahrzeitSeehafen2010)
}
