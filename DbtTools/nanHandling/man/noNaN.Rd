\name{noNaN}
\alias{noNaN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ remove NaNs }
\description{Removes all NaNs from input}
\usage{
noNaN(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ numeric vector or matrix }
}
\value{
  \item{elements}{the data without NaN or Infinity}
  \item{noNaNInd}{vector with the indecec of NaNs}
  \item{ nrElements }{number of final elements }
}

\author{ zm }

