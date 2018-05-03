trimstd <- function(Data,TrimPercentile){

 cases<-nrow(Data)
 vars<-ncol(Data)
 if(vars==1) {
    xlow <- quantile(Data,round(TrimPercentile/2)/100,type=5,na.rm=TRUE)
    xhi <- quantile(Data,round(100-TrimPercentile/2)/100,type=5,na.rm=TRUE) 
    densInd<-which(Data>=xlow & Data<=xhi)
    if( length(densInd)>0){
        trimmedSdev<-sd(Data[densInd])
    }else{
        trimmedSdev<-0
    }
 }else{
    trimmedSdev<-matrix(0,1,vars)
    for(i in 1:vars){
        xlow <- quantile(Data[,i],round(TrimPercentile/2/100),type=5,na.rm=TRUE)
        xhi<- quantile(Data[,i],round((100-TrimPercentile/2)/100),type=5,na.rm=TRUE)
        densInd<-which(Data[,i]>=xlow & Data[,i]<=xhi)
        if( length(densInd)>0){
            trimmedSdev[i]<-sd(Data[densInd,i])
        }else{
            trimmedSdev[i]<-0 
        }  
      }
  }
 return (trimmedSdev) 

 }

