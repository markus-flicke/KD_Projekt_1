\name{HarmonicDecay}
\alias{HarmonicDecay}

\title{
HarmonicDecay
}
\description{
 calculates the Harmonic Decay Numbers HarmonicDecay(i,n) for i=1...n
}
\usage{
HarmonicDecay(n)
}

\arguments{
  \item{n}{

}
}
\details{
 HarmonicDecay(i,n) = sum(1/k) , k=i...n
 
 Laws:
 HarmonicDecay(1,n) = Harmonic number HN(n) = 1+1/2+...1/n
 
                      this approximates to:  log(n)+ 0.5772156649+ 1/(2*n)
                      
 HarmonicDecay(n,n) = 1/n
}
\value{

HD(1:n)       HD(i) = HarmonicDecay(i,n)
}

\author{
Michael Thrun
}
\keyword{ HarmonicDecay }
