\name{countZero}
\alias{countZero}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Zeros counter }
\description{
  Count the zeros in one numeric vector or matrix.
}
\usage{
countZero(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ vector or matrix }
}
\value{
  The number of zeros in the data.
}

\author{ Zornitsa Manolova }

\examples{

x<-c(0:10)
countZero(x) #the output muss be 1

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ zero }
\keyword{ count }
\keyword{ find }
