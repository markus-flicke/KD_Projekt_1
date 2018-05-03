\name{dbt_spearman}
\alias{dbt_spearman}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Spearman's ranking }
\description{
  Calculate spearman rank correlation coefficient.
}
\usage{
dbt_spearman(x, y)
}
\arguments{
  \item{x}{ numeric vector }
  \item{y}{ numeric vector }
}
\details{
  The algorithm come from octave.
}
\value{
  \item{ rho }{rank correlation coefficient} 
}
\author{ Zornitsa Manolova }
\seealso{\code{\link{nanspearman}} }


\keyword{ spearman }
\keyword{ rank }
