nantoClassMean <- function(Data,Cls){

 temp<-ClassMean(Data,Cls)
 d<-dim(Data)
 rows<-c(1:d[1])
 
 x<-Data
 for(i in 1:d[1]){
     clsInd<-which(temp$uniqueClasses==Cls[rows[i]])
     classM<-temp$meanPerClass[clsInd,]
     nanInd<-which(is.na(x[rows[i],]), arr.ind =TRUE)
     x[rows[i],nanInd]=classM[nanInd]
 } 
 return (x) 

 }

