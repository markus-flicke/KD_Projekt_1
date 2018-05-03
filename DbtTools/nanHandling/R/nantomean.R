nantomean <-  function(z){

  out<-z
 if(is.matrix(out)){
    m<-colMeans(out,na.rm=TRUE)
    d<-dim(out)
    for(i in 1:d[2]){
        out[,i]<-nantovalue(out[,i],m[i])   
    }
 }else{
    m<-mean(out,na.rm=TRUE)
    out[is.na(out)]<-m
 } 
 return (out) 

 }

