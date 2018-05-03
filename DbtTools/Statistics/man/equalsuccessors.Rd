\name{equalsuccessors}
\alias{equalsuccessors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Equal Successors }
\description{
  Count the equal successors
}
\usage{
equalsuccessors(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{   numeric vector or matrix }
}
\details{
  Na and NaN values will be ignored
}
\value{
  \item{nrOfEquals}{   number of equal successors}
  \item{Indices}{   Indeces of the equal successors}
}

\author{ Zornitsa Manolova } 
\examples{
  x<-c(1,1,3,4,NA,4,4,NaN,4)
  equalsuccessors(x) #return 2
  x<-cbind(x,x)
  equalsuccessors(x) #return 4

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ equal }
\keyword{ successors }
\keyword{ count }
