\name{histopt}
\alias{histopt}
\title{ Histogram with optimal number of bins }
\description{
  Draws a histogram with the optimal number of bins
}
\usage{
  histopt(Data, Title, PlotIt, AxisLabs)
}
\arguments{
  \item{Data}{ Data input for the histogram }
  \item{Title}{ Optional: changes the title of the plot. Default is Histogram of data }
  \item{PlotIt}{ Optional: logical, whether to plot or not }
  \item{AxisLabs}{ Optional: logical value. FALSE will suppress the labeling of the axes }
}

\value{
    \item{nrOfBins}{the n cells }
    \item{nrInBins}{ amount in each bin: from x[1] to x[n]}
    \item{binMids}{the n cell midpoints.}
}
\references{
David W. Scott Jerome P. Keating: "A Primer on Density Estimation for the Great Home Run Race of 98", STATS 25, 1999, pp 16-22.
 }
\author{Michael Thrun, Rabea Griese}