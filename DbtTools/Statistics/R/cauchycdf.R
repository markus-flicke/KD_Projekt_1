cauchycdf <- function(Data,m=0,md=1){

 cdf<-dcauchy(Data, location=m, scale=1)
 plot(Data,cdf)
 return (cdf) 

 }

