nanmad = function(x){
#author: MT 07/2016
# x2=(as.matrix(x)-repmat(median(as.matrix(x),na.rm=TRUE),nrow(as.matrix(x)),ncol(as.matrix(x))))
 #m=apply(FUN=median,X = abs(x2),MARGIN=2,na.rm=T) 
  
  medianvec =function(x3){ 
  median(abs(x3-median(x3,na.rm=T)),na.rm=T)
  }
  
  if(is.vector(x)){
    return(medianvec(x))
  }else{
  
 m=apply(FUN=medianvec,X =x,MARGIN=2)
  } 

 return (m) 

 }