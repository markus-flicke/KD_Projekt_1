\name{nancor}
\alias{nancor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Matrix of Correlations }
\description{
  compute Pearsons correlation matrix for all columns. 
}
\usage{
nancor(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a matrix,that can contained NAs/NaNs}
}
\details{
  use rcorr function from the package "Hmisc"
}
\value{
  \item{corrmat }{correlation matrix}
  \item{n }{matrix with the number of used points}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ Zornista Manolova }
\note{uses the function \code{rcorr} from the 'Nsinc'-Pakage
}
\seealso{\code{\link{cor}}}
\examples{
     x<-matrix(c(1,NA,3,NA,5,1,2,NA,NA,5,1,NA,3,NA,5),ncol=3)
     nancor(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cor }
\keyword{ na }% __ONLY ONE__ keyword per line
