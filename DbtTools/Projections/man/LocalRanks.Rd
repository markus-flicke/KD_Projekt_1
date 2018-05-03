\name{LocalRanks}
\alias{LocalRanks}
\title{ die Raenge einer Indexmenge in einer Distanzmatrix ermitteln }
\usage{
LocalRanks(Ind, Distances, AnzInd = length(which(is.finite(Ind))))
}
\arguments{
\item{Ind}{Ind[i,] sind k indizes in Distance(1:n,1:n) oder NaN}
\item{Distances}{Matrix or vector of distances.}
\item{AnzInd}{AnzInd(i) ist die Anzahl der gueltigen Eintraege in Ind(i,:) wenn nicht angegeben  AnzInd = sum(isfinite(Ind),2);}
}
\value{Ranks(1:n,1:k)                      Ranks(i,:) sind die Raenge der Werte von Ind(i,:) in Distance(i,1:n)}
\description{
 die Raenge einer Indexmenge in einer Distanzmatrix ermitteln }
\author{
reimplemented from ALU's matlab version by MT}
