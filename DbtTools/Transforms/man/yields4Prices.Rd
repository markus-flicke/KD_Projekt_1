\name{yields4Prices}
\alias{yields4Prices}

\title{ yields for prices }
\description{
 This function calculates the yields of prices.}
\usage{
yields4Prices(prices, minPrice=0.01)
}

\arguments{
  \item{prices}{a matrix (rows: days, columns: quotation), prices on a daily basis in column}
  \item{minPrice}{ default = 0.01}
}

\value{
  \item{yield }{a matrix (rows: days, columns: quotation)}
  \item{averagePrices}{a matrix, containing the average price (rows: days, columns: quotation)}
}

\author{fm}
