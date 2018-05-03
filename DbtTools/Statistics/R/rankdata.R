rankdata <- function(x){

 ranks<- x*0
 rows<-nrow(x)
 cols<-ncol(x)
 for(i in 1:cols){
    tmp<-sort(na.last=NA,x[,i],index.return=TRUE);
    ss<-tmp$x; 
    sidx<-tmp$ix
    temp<-sort(na.last=NA,sidx,index.return=TRUE);
    ranks[,i]<-temp$ix 
 }

 return (ranks) 

 }

