\name{LeptoByKurtosis}
\alias{LeptoByKurtosis}

\title{ Lepto Kurtosis }
\description{
  Estimation if x is a distribution with fat tail using kurtosis.
}
\usage{
LeptoByKurtosis(x)
}

\arguments{
  \item{x}{ a matrix }
}
\value{
  \item{pLepto }{the probability that the distribution  of x is leptokurtic}
  \item{kurtosis}{a teststatistic == kurtosis(x)}
  \item{sKurt}{die Sdev fuer R, vllt aus  N(1.8567, Skurt)}
}
\author{ Zornitsa Manolova }
\seealso{ \code{\link{Leptokurtic}},\code{\link{Leptokurtic9010}} }

\keyword{ kurtic}
\keyword{ kurtosis}
\keyword{ leptokurtosis }
