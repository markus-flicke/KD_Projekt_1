\name{MeasureTundD}
\alias{MeasureTundD}
\title{
Computes rank-based smoothed precision and recall
}
\description{
Compares the projection in pData with the original data in 

Data and calculates trustworthiness and continuity of the 

projection for neighborhood sizes ranging from 1 to NeighborhoodSize
}
\usage{
MeasureTundD(Data, pData, NeighborhoodSize)
}
\arguments{
  \item{Data}{Matrix of original data}
  \item{pData}{Matrix of projected data}
  \item{NeighborhoodSize}{Sets the maximum number of neighbors to 
	
	calculate trustworthiness and continuity for}
}
\value{
Trustworthiness and Continuity for neighborhood sizes from 1 to NeighborhoodSize
}
\details{

A NOTE ABOUT THE OUTPUT OF measure

Here's a sample line of output from 'measure', the program used for 
computing trustworthiness and continuity:

20 0.000593553  0.000593553  0.000593553  0.015855  0.015855  0.015855

The first column (20) is just the number of neighbors used to calculate
trustworthiness and continuity. Column 2 is worst-case trustworthiness (see
next paragraph), column 4 is best-case trustworthiness, and column 3 is the
average of these two. Analogously, column 5 is worst-case continuity, column
7 is best-case continuity, and column 6 is the average of the two. (Actually
each column contains the number 1 - x, where x is the measure. For example,
the trustworthiness in this case is actually 1 - 0.000593553  = 0.9994.)
}
\references{
Venna, J., and Kaski, S. Neighborhood preservation in nonlinear projection methods: An experimental study Artificial Neural Networks—ICANN 2001 (pp. 485-491): Springer  (2001).

Samuel Kaski, Janne Nikkilä, Merja Oja, Jarkko Venna, Petri Törönen, and Eero Castren. Trustworthiness and metrics in visualizing similarity of gene expression. BMC Bioinformatics, 4:48, 2003.

}
\author{
Felix Pape
}