\name{WilcoxonTest}
\alias{WilcoxonTest}

\title{
Wilcoxon rank sum test for equal medians , needs a binary Cls
}
\description{
Wilcoxon rank sum test (equivalent to the Mann-Whitney) is carried out. The null hypothesis is that the distributions of x and y differ by a location shift. 
}
\usage{
WilcoxonTest(Data,Cls)
}
\arguments{
  \item{Data}{
d cases,  n variables
}
  \item{Cls}{
Cls(i) == ClusterNumber of Data(i,:)
}
}
\details{
 performs a two-sided rank sum test of the null hypothesis that data in the vectors x and y are independent samples from identical continuous distributions with equal medians, against the alternative that they do not have equal medians. x and y can have different lengths.

The literature is not unanimous about the definitions of the Wilcoxon rank sum and Mann-Whitney tests. The two most common definitions correspond to the sum of the ranks of the first sample with the minimum value subtracted or not: R subtracts and S-PLUS does not, giving a value which is larger by m(m+1)/2 for a first sample of size m. (It seems Wilcoxon's original paper used the unadjusted sum of the ranks but subsequent tables subtracted the minimum.
}
\value{
# Pvalues(1:n)             Pvalues(i) is the p-value from the test 
# UniqueClasses(1:2)       the  two classes  used in  unique classes in Cls
}
\references{
David F. Bauer (1972), Constructing confidence sets using rank statistics. Journal of the American Statistical Association 67, 687–690.

Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 27–33 (one-sample), 68–75 (two-sample).
Or second edition (1999).
}
\author{
Michael Thrun
}
\note{
calls \code{wilcox.test(x,y,alternative="two.sided",paired=F)}
}

\seealso{
 \code{\link{wilcox.test}}
}

\keyword{Wilcoxon rank sum test}
\keyword{Mann-Whitney U-test}
