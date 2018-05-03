\name{nantoknearest}
\alias{nantoknearest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ NaN to k-nearest }
\description{
  Replace NaNs in z with mean of k nearest neighbor in Data
}
\usage{
nantoknearest(z, k, Data, defined=rep(1,dim(Data)[2]))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ vector with NaNs }
  \item{k}{ number of nearest neigbors to use }
  \item{Data}{ matrix of size nxd, d=number of column }
  \item{defined}{ vector of length d with 1 for columns to be used
                  every column where defined is not 1 is discarded.
                  If defined is missing all components are used }
}
\details{
  Use the function "ktonearest"  from the package dbt.Distances
}
\value{
  \item{ n }{ vector with NaNs replaced }
}
\author{ Zornitsa Manolova }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ NA }
\keyword{ NaN }% __ONLY ONE__ keyword per line
\keyword{ replace }
\keyword{ substitute }