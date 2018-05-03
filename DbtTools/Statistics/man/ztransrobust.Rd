\name{ztransrobust}
\alias{ztransrobust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Z-Transform Robust }
\description{
  Z-transform for the x-values from the data; use meanrobust and stdrobust
}
\usage{
ztransrobust(x, p=0.2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a numerical vector }
  \item{p}{ optional; Percent of the total cut }
}
\details{ use repmat, meanrobust and stdrobust}
\value{
  \item{z }{a data matrix}
}
\author{ Zornitsa Manolova }
\seealso{ See Also as \code{\link{repmat}}, \code{\link{stdrobust}},\code{\link{meanrobust}} }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
