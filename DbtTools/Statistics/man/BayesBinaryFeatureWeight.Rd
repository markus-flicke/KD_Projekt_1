\name{BayesBinaryFeatureWeight}
\alias{BayesBinaryFeatureWeight}
\title{
Classification weight for a binary classification
}
\description{
Classification weight for a binary classification 
according  to Duda et al  Pattern Classification pp 52,53
w = log((p * (1-q)) / (q *(1-p)) )
with  p = frequency of BinaryVariable==1 for Cls == c1
and   q = frequency of BinaryVariable==1 for Cls == c2
}
\usage{
BayesBinaryFeatureWeight(BinaryVariable, Cls)
}
\arguments{
  \item{BinaryVariable}{
a binary variable consiting of only two values, 0 and 1, may contain NaN 
}
  \item{Cls}{
a classification vector consiting of two classes c1 and c2, may contain NaN
}
}
\author{
Michael Thrun
}
