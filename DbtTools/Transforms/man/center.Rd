\name{center}
\alias{center}

\title{center}
\description{
 This function subtracts the column-mean from a matrix.
}

\usage{
center(x)
}
\arguments{
  \item{x}{numeric matrix.}
}

\value{
  Returns a numeric matrix.
}


\seealso{\code{\link{mean}}, \code{\link{colMeans}} for row and column means.}

\author{fm}


\examples{

data <- matrix(rnorm(50),10,5)
center(data)
}


\keyword{substract mean}