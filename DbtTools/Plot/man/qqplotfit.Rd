\name{qqplotfit}
\alias{qqplotfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ QQPlotFit }
\description{
  Plots empirical quantile vs empirical quantile with best-fit-line
}
\usage{
qqplotfit(x, y, PlotSymbol, xug, xog, LineWidth, PointWidth, xlabel, ylabel, mtitle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ x-axis data points for qq-plot }
  \item{y}{ y-axis data points for qq-plot~ }
  \item{PlotSymbol}{ Optional: symbol used to plot }
  \item{xug}{ Optional: lower limit of the interval [xug, xog], in which will be interpolated: default=min(x)  }
  \item{xog}{ Optional: upper limit of the interval [xug, xog], in which will be interpolated: default=max(x) }
  \item{LineWidth}{ Optional: Line width for the plot; default=1 }
  \item{PointWidth}{ Optional: Size of the points; default = 1}
  \item{xlabel}{Optional: Title of the x axis}
  \item{ylabel}{Optional: Title of the y axis}
  \item{mtitle}{Optional: Title of the plot}
}
\details{    }
\value{   
  \item{delta}{Differences between line and plot at the interpolation points}
  \item{meanAbsDelta}{sum(abs(Delta))/NumOfPoints}    
}
\references{  }
\author{databionics UnivMarburg}
\note{   }
\seealso{  }
\examples{ }

\keyword{ plot }
\keyword{ qqplot }
\keyword{ qq-plot }
\keyword{ quantile/quantile-plot }
\keyword{ best-fit-line }