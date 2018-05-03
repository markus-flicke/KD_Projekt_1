\name{nanspearman}
\alias{nanspearman}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Spearman's ranking ignoring missing values }
\description{
  Calculate column by column spearman rank correlation coefficient ignoring missing values(NA and NaN).
}
\usage{
nanspearman(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{   numeric vector }
}

\value{
  \item{ C }{   rank correlation matrix }
  \item{ N }{   number of data points used }

}
\author{ Zornitsa Manolova }
\seealso{\code{\link{spearman}} }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spearman }
\keyword{ rank }
