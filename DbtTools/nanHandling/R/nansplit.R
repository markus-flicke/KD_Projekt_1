nansplit <- function(x){

 nrofnan<-rowSums(is.na(x))
 temp<-sort(na.last=NA,nrofnan,index.return=TRUE) 
 xx<-x[temp$ix,]
 z<-sum(nrofnan==0)
 complete<-xx[1:z,]
 withNaN<-xx[(z+1):nrow(xx),]
 return (list(complete=complete,withNaN=withNaN)) 

 }

