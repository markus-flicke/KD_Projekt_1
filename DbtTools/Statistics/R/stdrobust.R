stdrobust <- function(x,lowInnerPercentile=25){

  if(is.vector(x) || (is.matrix(x) && dim(x)[1]==1)) dim(x)<-c(length(x),1)
  
  lowInnerPercentile<-max(1,min(lowInnerPercentile,49))
  hiInnerPercentile<- 100 - lowInnerPercentile
  faktor<-sum(abs(qnorm(t(c(lowInnerPercentile,hiInnerPercentile)/100),0,1)))
  std<-sd(x,na.rm=TRUE)
  p<-c(lowInnerPercentile,hiInnerPercentile)/100
  quartile<-prctile(x,p)  
  if (ncol(x)>1)
    iqr<-quartile[2,]-quartile[1,]
  else
    iqr<-quartile[2]-quartile[1]
  
  shat<-c()
  for(i in 1:ncol(x)){
      shat[i]<-min(std[i],iqr[i]/faktor,na.rm=TRUE)
  }
  dim(shat)<-c(1,ncol(x))
  colnames(shat)<-colnames(x)
  return (shat) 

 }

