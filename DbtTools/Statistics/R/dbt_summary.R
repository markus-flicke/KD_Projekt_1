dbt_summary <-function(Data){


   m<-colMeans(as.matrix(Data),na.rm=TRUE)
   minimum<-apply(as.matrix(Data),2,min,na.rm=TRUE)
   maximum<-apply(as.matrix(Data),2,max,na.rm=TRUE)
   med<-apply(as.matrix(Data),2,median,na.rm=TRUE)   
   nrNans<-nans(as.matrix(Data))$nrOfNaNs   
 
 std<-sd(as.matrix(Data),na.rm=TRUE)
 titles<-c("Number of NaNs", "min", "mean-sd", "mean", "mean+sd", "median", "max")
 elements<-cbind(nrNans,minimum,m-std,m,m+std,med,maximum)
 colnames(elements)=titles
 return (elements) 

 }