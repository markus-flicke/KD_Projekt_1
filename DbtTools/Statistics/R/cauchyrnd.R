cauchyrnd <- function(ml, md=1, m, n=1){

 if (is.vector(m)) m = length(m)
 if (md<0) md=NaN
 R=cauchyinv(matrix(runif(m*n),nrow=m,ncol=n),ml,md)
 return (R) 

 }

