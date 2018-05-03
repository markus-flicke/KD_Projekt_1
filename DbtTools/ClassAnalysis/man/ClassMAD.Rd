\name{ClassMAD}
\alias{ClassMAD}

\title{
Median Absolute Deviation (MAD)~
}
\description{
Median Absolute Deviation (MAD)  in each group of the Data
#  NaN values in Data are ignored
}
\usage{
ClassMAD(Data,Cls)
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
  \item{MADPerClass}{(AnzClass,n)       MADPerClass(i) is the median Absolute Deviation (MAD) of the Data points in UniqueClasses(i)
 }

}

\author{
Michael Thrun
}


\keyword{MAD}
