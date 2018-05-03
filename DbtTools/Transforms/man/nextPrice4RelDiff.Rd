\name{nextPrice4RelDiff}
\alias{nextPrice4RelDiff}

\title{next price for relDiff}
\description{
  This function calculate the present price from the price of yesterday and RelDiff.
}

\usage{
nextPrice4RelDiff(priceYesterday, relDiff)
}

\arguments{
  \item{priceYesterday}{prices of yesterday, on a daily basis column by column}
  \item{relDiff}{ relative differences, on a daily basis column by column}
}

\value{
  returns the prices of today, on a daily basis column by column
}

\author{fm}
