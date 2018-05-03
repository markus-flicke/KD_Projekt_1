symlogrnd <- function(M,S,rows,cols){

 temp <- symlognSigmaMue(M,S)
 mu <- temp$mu
 sig <- temp$sig
 n <- rows*cols
 randomSymLog <- matrix(sign(M) * rlnorm(n, abs(mu), sig), rows, cols)
 
 return(randomSymLog) 
 }

