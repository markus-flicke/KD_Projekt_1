\name{UDLtrans}
\alias{UDLtrans}

\title{ UDL Transformation}
\description{
  This function projects the lowest and highest values of x such that a uniform density is reached.
  Uses a linear appoximation of the central parts of cdftrans(x).
}
\usage{
UDLtrans(x, plotIt='FALSE', lowHighAll='FALSE', lowLim=0.02, hiLim=0.98)
}


\arguments{
  \item{x}{a vector or matrix containing the values to be transformed, dim(m,n) m >=10 }
  \item{plotIt}{ logical. Plot the approximation? }
  \item{lowHighAll}{If 0: the transformation is applied ouside the intercal [LowLim,HiLim] of cdftrans(x), \cr
	if 1: only the low  values of x are transformed, \cr
	if 2: only the high values of x are transformed, \cr
	if 3: all the x values are adapted }
  \item{lowLim}{ lower bound of the interval}
  \item{hiLim}{ upper bound of the interval}
}


\author{ fm }
