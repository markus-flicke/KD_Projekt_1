nantomedian <- function(z){

 out<-as.matrix(z)
 m<-apply(out,2,median,na.rm=TRUE)
 for(i in 1:dim(out)[2]){
    out[,i]<-nantovalue(out[,i],m[i])
 }
 return (out) 

 }

