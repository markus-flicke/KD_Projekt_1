\name{ClassPlot}
\alias{ClassPlot}
\title{
plot XY data colored by Cls
}
\description{
plot XY data colored by Cls
}
\usage{
ClassPlot(X, Y, Cls = X * 0, Title = " ", Xlabel = "X", Ylabel = "Y", xlim = c(), ylim = c(), ColorSequence, ColorSymbSequence, PlotLegend = 1, TilePlots = c(),add = F, \dots)
}
\arguments{
  \item{X}{
  the coordinates of the data
}
  \item{Y}{
  the coordinates of the data
}
  \item{Cls}{
  integer vector of class identifiers 
}
  \item{Title}{
  Optinal: Title of the plot
}
  \item{Xlabel}{
  Optional: Label of the x axis
}
  \item{Ylabel}{
  Optional: Label of the y axis
}
  \item{xlim}{
  Optional: range of the x axis
}
  \item{ylim}{
  Optional: range of the y axis
}
  \item{ColorSequence}{
  Optional: the sequence of colors used, Default: DefaultColorSequence
}
  \item{ColorSymbSequence}{
  Optional: the plot symbols used (theoretisch nicht notwendig, da erst wichtig, wenn mehr als 562 Cluster)
}
  \item{PlotLegend}{
  Optional: add a legent to plot (default == 1)
}
  \item{TilePlots}{
  Optinal: [l,c] if given a tile plot tileplot(l,c,*) for each class separately is given. NOT JET IMPLEMENTED!
}
  \item{add}{
  defaul false, 
}
  \item{\dots}{
  further arguments passed to plot  
}

}
\value{
None
}
\author{
Willi Eggeling
}