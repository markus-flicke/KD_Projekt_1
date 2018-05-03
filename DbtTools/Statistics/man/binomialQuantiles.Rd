\name{binomialQuantiles}
\alias{binomialQuantiles}
\title{ Binomial Quantiles }
\description{
  Calculate the limit values for wich is significant a random choise of 
  one subset with certain Size.
}
\usage{
binomialQuantiles(ns, p, alpha=0.99)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{elements}{  number of Elements in the subset }
  \item{p}{   apriori probability }
  \item{alpha}{   probability for random result. DEFAULT alpha=1\% }
}

\value{    
  \item{lowerPercent}{  The result for each subset is significant after falling beneath that percentage }
  \item{upperPercent}{  The result for each subset is significant after exceeding that percentage } 
  \item{lowerNumber}{   upper bound for the number of Data}
  \item{upperNumber}{   lower bound for the number of Date}  
}
\references{ Idea by F. Moerchen, September 2002 }
\author{ Zornitsa Manolova }
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ binomial }
\keyword{ quantile }
