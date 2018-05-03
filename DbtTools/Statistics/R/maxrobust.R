maxrobust <- function(x){

  maxX<-max(x)
  prctile<-99
  pMax<-quantile(x,prctile/100,type=5,na.rm=TRUE)
  if(maxX>min(x)){
      while(maxX==pMax){
        prctile<-prctile-1
        pMax<-quantile(x,prctile/100,type=5,na.rm=TRUE)
      }
      maxhat<-mean(cbind(maxX,pMax))
  }else{
      maxhat<-maxX
  }
 return (maxhat) 

 }

