\name{dbt_scatterplot}
\alias{dbt_scatterplot}
\title{Creates a scatterplot for data}
\usage{dbt_scatterplot(Data, Names="", cls=NULL)}
\arguments{
  \item{Data}{Matrix of points to be scatterplotted}
  \item{Names}{Optional: Names of the variables in Data}
  \item{cls}{Optional: Class vector assigning classes to points}
}
\description{ Creates scatterplots for each column against every other column. Splits to multiple grapic devices if too big.}
