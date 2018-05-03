\name{ClassEntropy}
\alias{ClassEntropy}
\title{ Class Entropy}
\description{
Calculates the entropy.}
\usage{
ClassEntropy(cls, LogBasis)
}
\arguments{
  \item{cls}{ cls(i) is the clusterNumber of a given data(i)}
  \item{LogBasis}{Optional: base of the logatithm used to calculate entropy, default: 2}
}
\value{
\item{entropy}{Overall entropy, the sum of entropyPerClass. }
\item{entropyPerClass}{Partial entropy per unique classes. }
\item{uniqueClass}{Ordered classes }
\item{p}{Vector of partial probabilities corresponding to uniqueClasses}
\item{normalizedEntropy}{ entropy / uniformEntropy, in order to compare entropy of different class numbers. }
\item{uniformEntropy}{The entropy of a classification with the same number of classes as cls but with the same number of elements in each class.}
}
% \references{ ~put references to the literature/web site here ~ }
\author{ Raphael Paebst}
% \note{ ~~further notes~~ }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
% \examples{}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
