\name{ProjectedPoints2Grid}
\alias{ProjectedPoints2Grid}
\title{ ProjectedPoints2Grid(ProjectedPoints,Lines,Columns) Converts numeric ProjectedPoints to integervalues corresponding to a two dimensional rectangular grid}
\arguments{
\item{ProjectedPoints[n,1:2] or [n,1:3] matrix containing the number of n coordinates !(x,y)! of the Projection}{ if [n,1:3] the first column has to be the key}
\item{Lines}{Default(50), widght of the retangular grid}
\item{Columns}{height of the rectangular grid We are able to calculate Columns by using Lines and der Range of Data in x and y}
\item{PlotIt}{Difference plottet in new Window, if TRUE}
}
\value{BestMatches[n,Key,Lines,Columns]  Integer GridPositons(1:Lines,1:Colums) corresponding to numerical !(y,x)! coordinates}
\description{
 ProjectedPoints2Grid(ProjectedPoints,Lines,Columns) Converts numeric ProjectedPoints to integervalues corresponding to a two dimensional rectangular grid}
\author{
MT 07/2015}
