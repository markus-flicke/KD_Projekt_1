\name{quantilNormalize}
\alias{quantilNormalize}


\title{Normalize Quantile}
\description{
 This function normalizes quantile. Data are replaced in accordance with their rank by the values of the ReferenceDistribution.
}
\usage{
quantilNormalize(data, standardized=1, referenceDistribution)
}

\arguments{
  \item{data}{numeric matrix. Observations in lines, variables in rows. } 
  \item{standardized}{the reference distribution will be z-transformed. }
  \item{referenceDistribution}{numeric vector.}
}

\value{
  returns the quantile normalized data 
}


\author{fm}
