modus <- function(x){

 n<-length(x)
 temp<-x
 dim(temp)<-c(n,1)
 xu<-unique(temp)
 m<-length(xu)
 dim(xu)<-c(1,m)
 A<-matrix(1,nrow=n,ncol=1)%*%xu
 B<-temp%*%matrix(1,nrow=1,ncol=m)
 enumber<-colSums(A==B)
 temp<-sort(na.last=NA,enumber,decreasing=TRUE,index.return=TRUE)
 enumber<-temp$x
 elements<-xu[temp$ix]
 modus<-elements[1]
 mnumber<-enumber[1]
 
 return (list(modus=modus,mnumber=mnumber,elements=elements,enumber=enumber)) 

 }

