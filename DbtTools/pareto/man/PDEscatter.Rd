\name{PDEscatter}
\alias{PDEscatter}
\title{PDE scatter}
\description{
This function generates the density of the xy data as a z coordinate. Afterwards xyz will be plotted either as a contour plot or a 3d plot.
}
\usage{
PDEscatter(x,y,paretoRadius=0,maximumNrSamples=1000,nrOfContourLines=20, Xlab="X", Ylab="Y", Legendlab="value", Plotter='native', DrawTopView = T)
}

\arguments{
  \item{x}{Numeric vector, data in x dimension.}
  \item{y}{Numeric vector, data in y dimension.}
  \item{paretoRadius}{Numeric, the Pareto Radius. If omitted (or 0), calculate by paretoRad.}
  \item{maximumNrSamples}{Numeric, maximum size of the sample used for calculation. High values increase runtime significantly.}
  \item{nrOfContourLines}{Numeric, number of contour lines to be drawn. 20 by default.}
  \item{Xlab}{String, title of the x axis. Default: "X"}
  \item{Ylab}{String, title of the y axis. Default: "Y"}
  \item{Legendlab}{String, label for the legend. Default: "value"}
  \item{Plotter}{String, name of the plotting backend to use. Possible values are: "native", "ggplot", "plotly"}
  \item{DrawTopView}{Boolean, True means contur is drawn, otherwise a 3D plot is drawn. Default: True}
}
\details{
This function plots the PDE on top of a scatterplot. Variances of x and y should not differ by extreme numbers, otherwise use toPercent() on both first.
}
\value{
 List of:
  \item{AnzInPSpheres}{Numer of points within the ParetoRadius of each point}
  \item{ParetoRadius}{ParetoRadius used for PDEscatter}
  \item{Handle}{Handle of the plot object. NULL if native R plot is used}

}
\references{Matlab dbt - Pareto}
\author{Felix Pape}
