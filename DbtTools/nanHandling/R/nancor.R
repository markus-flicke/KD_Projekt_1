nancor <- function(x){

# library('Hmisc')
requireNamespace('Hmisc')
 out<-Hmisc::rcorr(x)
 r<-out$r
 out$r[is.na(out$r)]<-0
 return (list(corrmat=out$r,n=out$n)) 

 }

