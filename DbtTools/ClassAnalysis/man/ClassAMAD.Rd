\name{ClassAMAD}
\alias{ClassAMAD}

\title{
Median Absolute Deviation (MAD)
}
\description{

NaMedian Absolute Deviation (MAD)  in each group of the DataN values in Data are ignored
}
\usage{
ClassAMAD(Data,Cls)
}

\arguments{
  \item{Data}{
d cases,  n variables
}
  \item{Cls}{
Cls(i) == ClusterNumber of Data(i,:)
}
}

\value{
 \item{UniqueClasses}{(AnzClass)      the  AnzClass unique classes in Cls
 }
  \item{AMADPerClass}{(AnzClass,n)      MADPerClass(i) is the median Absolute Deviation (MAD) of the Data points in UniqueClasses(i)
 }

}
\references{
\url{https://en.wikipedia.org/wiki/Median_absolute_deviation}
}
\author{
Michael Thrun
}

\seealso{
\code{\link{ClassMAD}}
}

\keyword{AMAD}% use one of  RShowDoc("KEYWORDS")
