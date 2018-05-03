\name{toRange}
\alias{toRange}

\title{ to range}
\description{
  This function scales data columnwise to range [lower, upper], ignores NaNs.
}
\usage{
toRange(data, lower, upper)
}

\arguments{
  \item{data}{ numeric matrix containing the data to range. }
  \item{lower}{ lower border of interval.  }
  \item{upper}{ upper border of interval.  }
}

\value{
  returns the scaled data matrix.
}

\author{fm}
