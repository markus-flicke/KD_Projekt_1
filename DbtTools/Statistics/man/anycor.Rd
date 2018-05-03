\name{anycor}
\alias{anycor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Correlation Values }
\description{
  Check if any high correlation values in matrix created with e.g. nanspearman.R
}
\usage{
anycor(CorM, tr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CorM}{ correlation matrix }
  \item{tr}{ threshold }
}

\value{   
  \item{ b }{ boolean; true if any values > t except on diagonal}
}
\author{ Zornitsa Manolova }
\seealso{  \code{\link{nanspearman}} }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }
\keyword{ nanspearman }% __ONLY ONE__ keyword per line
