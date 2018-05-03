\name{logRatio4Prices}
\alias{logRatio4Prices}

\title{ LogRatio for prices }
\description{
  The function transforms prices into LogRatios. 
}
\usage{
logRatio4Prices(prices, minPrice = 0.01)
}

\arguments{

  \item{prices}{ a matrix (rows: days, columns: quotation), prices on a daily basis row by row.} 
  \item{minPrice}{ 0.01 by default (1 Cent). }
}

\value{
returns a matrix containing the log of yield.
}

\author{fm}
