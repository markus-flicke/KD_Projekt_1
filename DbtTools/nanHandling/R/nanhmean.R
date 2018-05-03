nanhmean <- function(x){

 xx<-x[!is.na(x) & x!=0]
 m = hmean(xx)
 return (m) 

 }

