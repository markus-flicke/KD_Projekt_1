\name{dbt_qqplot}
\alias{dbt_qqplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ QQ Plot }
\description{
  Plots empirical quantile vs empirical quantile
}
\usage{
dbt_qqplot(x, y, pstyle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ X-axis data points for qq-plot }
  \item{y}{ Y-axis data points for qq-plot }
  \item{pstyle}{ Optional: symbol used to plot }
}
\value{
  \item{px}{percentiles(x)}
  \item{py}{percentiles(y)}    
}
\author{databionics UnivMarburg}
