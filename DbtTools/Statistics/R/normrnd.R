normrnd <- function(mu, sigma, m, n){
# generates random numbers from normal distribution with mean parameter mu and standart deviation parameter sigma

# INPUT
# mu		mean parameter
# sigma		standart deviation parameter
# n			number of rows of OUTPUT matrix
# m			number of cols of OUTPUT matrix

# OUTPUT
# NormMat	matrix(n,m) filled with random numbers from normal distribution

# 1.editor: 06/2015 RG
  
#rnorm(n, mean = 0, sd = 1)
x <- n*m
NormMat <- matrix(rnorm(x, mu, sigma), m, n) #RG: m und n waren vertauscht

return(NormMat)
}