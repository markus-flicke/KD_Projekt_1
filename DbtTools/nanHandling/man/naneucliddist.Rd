\name{naneucliddist}
\alias{naneucliddist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Naneucliddist }
\description{
  Euclidean distances between vectors containing NaNs or NAs
}
\usage{
naneucliddist(Data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{ the Data, cases in rows, variables in colums }
}

\value{
  \item{vectorOfDistances }{distance vector}
  \item{distanceMatrix }{symmetric distance matrix with zeros along the diagonal}

}
\author{ Zornitsa Manolova }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distances }
\keyword{ euclidian }% __ONLY ONE__ keyword per line
