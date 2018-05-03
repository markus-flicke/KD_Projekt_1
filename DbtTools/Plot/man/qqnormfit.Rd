\name{qqnormfit}
\alias{qqnormfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ QQNormFit }
\description{
  Quantile/Quantile-Plot with best-fit-line
}
\usage{
qqnormfit(x, xug, xog, pstyle, PlotIt, Symbol4Gerade, xlab, ylab, main, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Variable to be drawn }
  \item{xug}{ Optional: lower limit of the interval [xug, xog], in which a line will be interpolated }
  \item{xog}{ Optional: upper limit of the interval [xug, xog], in which a line will be interpolated }
  \item{ylab}{ Optional: description of the y-axis; 'data' if not declared }
  \item{xlab}{ Optional: descriptuib if the x-axis; 'normal' if not declared}
  \item{pstyle}{ Optional: plot symbol. Default is 20. }
  \item{PlotIt}{ Optional: logical value, specifying whether to plot }
  \item{Symbol4Gerade}{ Optional: plot symbol for the best-fit-line; }
  \item{main}{Optional: Title of the plot}
  \item{\dots}{Optional: further arguments passed to qqnorm}
}
\author{databionics UnivMarburg}
