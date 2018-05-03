\name{DelaunayClassificationError}
\alias{DelaunayClassificationError}
\title{
DelaunayClassificationError
}
\description{
error=DelaunayClassificationError(Data,ProjectedPoints,Cls)
 sucht die k Naechsten Mit Euklidgewichteten DelaunayNachbahr und sucht alle Zellen, welche nicht

}
\usage{
DelaunayClassificationError(Data,ProjectedPoints,Cls,LC=NULL)
}
\arguments{
  \item{Data}{
[1:n,1:d]
}
  \item{ProjectedPoints}{
[1:n,1:2]
}
  \item{Cls}{
[1:n,1]
}
  \item{LC}{
Optional, default NULL,  Wenn toroid, muss c(Lines,Columns) angeben werden
}
}
\details{
see chapter 6 of [Thrun, 2016]
}
\value{
list of 

 DCE                            DelaunayClassificationError 
                                NOTE the rest is just for development purposes
																
 DCEperPoint(1:n)               unnormalized DCE of each point: DCE = mean( DCEperPoint)
 
 nn                             the number of points in a relevant neghborhood: 0.5 * 85percentile(AnzNN)
 
 AnzNN(1:n)                     the number of points with a delaunay graph neighborhood
 
 NNdists(1:n,1:nn)              the distances within the relevant nehborhoot, 0 for inner cluster distances
 
 HD(1:nn)                       HD = HarmonicDecay(nn) i.e weight function for the NNdists: DCEperPoint = HD*NNdists
 

}
\references{
Thrun, M. C.:A System for Projection Based Clustering through Self-Organization and Swarm Intelligence, (Doctoral dissertation), Philipps-Universität Marburg, Marburg, 2016.
}
\author{
Michael Thrun
}

\keyword{Classification Error}
