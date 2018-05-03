\name{ClassRepraesentant}
\alias{ClassRepraesentant}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ClassRepraesentant
}
\description{
 Representatnten suchen: dies sind die Mitglieder in den jeweiligen Klassen, die die groessten silhoutte wert haben

}
\usage{
ClassRepraesentant(Data,Cls,NumberOfRep=1) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
 d cases,  n variables
}
  \item{Cls}{
Cls(i) == ClusterNumber of data(i,:)
}
  \item{NumberOfRep}{
 number of repesentative items per class
 
  default only one representant per class
	
}
}
\value{
list V with

 Classes                     the  class numbers in in cls

 RepInd                      Data(RepInd(i))is the repraesentant of the data points in  Classes(i)

}

\author{
MT
}
