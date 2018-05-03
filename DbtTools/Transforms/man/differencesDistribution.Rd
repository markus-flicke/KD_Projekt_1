\name{differencesDistribution}
\alias{differencesDistribution}

\title{ differences distribution }
\description{
  The function calculates distances of distribution.
}

\usage{
differencesDistribution(feature, plotIt = FALSE)
}

\arguments{
  \item{feature}{numeric vector, the feature to analyze.}
  \item{plotIt}{logical. Do PDE plots?}
}

\details{

Assumption: The featuredifferences consist of a mix of distribution. 

Method: 
The smallest 10\% of the differences are used to get the sdev from the smallest differences (innerDerivation).
If the differences are normaldistributed the common sdev will return.

Required functions: pdeNorm, ParetoRadius, PDEstimationForGauss
}

\value{

  \item{innerDeviation}{estimation of the sdev of small differences.}
  \item{differences }{a subsample of differences in 'feature', at most 30.000 points as a symmetric distribution. }

}

\author{fm}

