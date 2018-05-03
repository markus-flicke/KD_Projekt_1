nangmean <-  function(x){

 xx<-x[!is.na(x) & x>0]
 m<-gmean(xx)
 return (m) 

 }

