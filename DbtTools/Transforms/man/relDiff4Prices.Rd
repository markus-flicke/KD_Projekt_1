\name{relDiff4Prices}
\alias{relDiff4Prices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ RelDiff for prices }
\description{
  This function transforms prices into RelDiff
}
\usage{
relDiff4Prices(prices, minPrice=0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prices}{prices on a daily basis column by column }
  \item{minPrice}{minPrices=0.01 by default}
}

\value{
  returns a matrix of RelDiffs (rows: days, columns: quotation)
}

\author{fm }