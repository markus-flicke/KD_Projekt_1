\name{ClassApply}
\alias{ClassApply}
\title{Applies a function over grouped data}
\description{
Applies a given function to each dimension for each class in Cls over the data.}
\usage{
ClassApply(Data,Cls,Func,\ldots)
}
\arguments{
  \item{Data}{ A matrix containing data. Cases as rows, variables as columns.}
  \item{Cls}{ The class identifiers for all rows in Data.}
  \item{Func}{Function to be applied to each class.}
  \item{\ldots}{Additional parameters to be passed to Func}
}
% \details{}
\value{
\item{UniqueClasses}{The unique classes in cls}
\item{ResultPerClass}{\code{ResultPerClass[i]} is the result of func for the data points in class \code{UniqueClasses[i]}}
}
\author{Felix Pape}