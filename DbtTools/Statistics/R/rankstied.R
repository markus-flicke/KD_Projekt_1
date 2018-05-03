rankstied <- function(x){

 y<-c()
 rows<-nrow(x)
 cols<-ncol(x)
 if(rows==1 && cols>0){
    p<-rep(t(x),cols)
    y<-sum((p<t(p))+sum(p==t(p))+1)/2
 }else{
    if(rows>1){
        for(i in 1:rows){
            p<-rep(x[,i],rows)
            y<-cbind(y,t(sum((p<t(p))+sum(p==t(p))+1)/2))
        }
    }
 }
 return (y) 

 }

