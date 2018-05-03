LogBinomialCoeff <- function(nIn,kIn){
# LogBino = LogBinomialCoeff( n, k )
# LogBinomialCoeff computes the (natural) logarithm of the Binomial coefficient.
# LogBino = log( binomialcoeff(N,K) ) =log(n!/( k! * (n-k)!));
#
# INPUT
# nIn, kIn  for binoialcoeff n over k
#       both can  be vectors
# 
# OUTPUT
# LogBino  the logarithm of binoialcoeff(N,K).
# USES 
# LogFactorial(n)
# 
#  Author:   John Burkardt
#  Source: http://people.sc.fsu.edu/~jburkardt/m_src/prob/prob.html
#  ALU Modifications transfered to dbt by MT
#  vaiable Names & vectorisation
# Edit: CL - Fall, dass k groesser n ist abgefangen

n = as.vector(nIn)
k = as.vector(kIn)


LogBino <- LogFactorial(n) - LogFactorial(k) - LogFactorial(n-k)
kgroessernInd <- which(n-k < 0) # Falls k > n, auf minus unendlich setzen, da Binomial(n,k) := 0, fuer k>n.
LogBino[kgroessernInd] <- -Inf


return(LogBino)
}