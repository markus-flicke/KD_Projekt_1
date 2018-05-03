medianNaN <-function(x){

 m<-apply(as.matrix(x),2,median,na.rm=TRUE) 
 return (m) 

 }

