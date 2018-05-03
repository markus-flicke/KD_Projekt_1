dbt_mad <- function(x){ 

 if(is.vector(x)){
    centerMad=mad(x);
    leftMad=mad(x[x<median(x)])
    rightMad=mad(x[x>median(x)])
 } 
  else{    
    centerMad<-matrix(1,ncol=ncol(x))
    leftMad<-matrix(1,ncol=ncol(x))
    rightMad<-matrix(1,ncol=ncol(x))
    for(i in 1:ncol(x)){        
    centerMad[,i]=mad(x[,i],na.rm=TRUE)
    leftMad[,i]=mad(x[x<median(x[,i])],na.rm=TRUE)
    rightMad[,i]=mad(x[x>median(x[,i])],na.rm=TRUE)    
    }
  }
 return (list(centerMad=centerMad,leftMad=leftMad,rightMad=rightMad)) 

 }

