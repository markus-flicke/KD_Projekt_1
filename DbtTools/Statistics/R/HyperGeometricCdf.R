HyperGeometricCdf <- function( xIn, nIn, mIn, lIn){
# cdf = HyperGeometricCdf( x, n, m, l )
# hypergeometric_cdf evaluates the Hypergeometric CDF.
#
# INPUT
# xIn[1:i]        the argument of the CDF                                MATLAB,hygecdf(X,M,K,N): X
# nIn[1:i]        number of balls selected 0 <= n <= l                   MATLAB,hygecdf(X,M,K,N): N
# mIn[1:i]        number of white balls in the population:  0 <= m <= l  MATLAB,hygecdf(X,M,K,N): K
# lIn[1:i]        the number of balls to select from  0 <= l             MATLAB,hygecdf(X,M,K,N): M
#     i>=1
#
# OUTPUT
# cdf          the value of the Hypergeometric CDF at x
#
# USES 
# LogBinomialCoeff(n,k)
# 
# Author: John Burkardt
#  Source: http://people.sc.fsu.edu/~jburkardt/m_src/prob/prob.html
#  ALUs modifications transfered to dbt by MT:
#  dbt documentation 
#  vectorized

x =  as.vector(xIn)
n =  as.vector(nIn)
m =  as.vector(mIn)
l =  as.vector(lIn)

c1_log = LogBinomialCoeff( l - m, n )
c2_log = LogBinomialCoeff( l, n )

pdf = exp( c1_log - c2_log )
cdf = pdf;  # init der Summation

AnzInVector = length(x)
if (AnzInVector<2){ # nur ein wert
    for (x2 in 0:(x - 1)){
    pdf = pdf * ( m - x2 ) * ( n - x2 ) / ( ( x2 + 1 ) * ( l - m - n + x2 + 1 ) )
    cdf = cdf + pdf
    } # for x2
}else{ # mehrere x werte
    for (i in 1:AnzInVector){
        for (x2 in 0:(x[i]-1)){
        pdf[i] = pdf[i]*(m[i]-x2)*(n[i]-x2) / ( ( x2 + 1 ) * (l[i]-m[i]-n[i]+x2 + 1 ) )
        cdf[i] = cdf[i] + pdf[i]
        } # for x2
    } # for i
} # if AnzInVector<2 % nur ein wert
return(cdf)
}