\name{cdftrans}
\alias{cdftrans}

\title{ cdf transformation}

\description{
  This function transforms data to its empirical cumulative density.
}

\usage{
cdftrans(x)
}

\arguments{
  \item{x}{ numeric matrix with data to transform.}
}

\value{
 Returns a numeric matrix with the empirical cumulative density of x.
}


\author{ fm }



\seealso{\code{\link{ecdf}}}

\examples{


data <- matrix(rnorm(50),10,5)
cdftrans(data)

}

\keyword{cdf}

