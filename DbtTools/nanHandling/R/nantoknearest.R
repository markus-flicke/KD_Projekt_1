nantoknearest <- function(z,k,Data,defined=rep(1,dim(Data)[2])){

 d<-dim(Data)
 naninz<-ifelse(is.na(z),1,0)
 defined<-max(0,defined-naninz)
 nanind<-c(1:length(naninz))
 z[naninz]<-0
 nn<-knneighbor(z,k,Data,defined)
 if(k>1){
    m<-mean(Data[nn,])
 }else{
    m<-Data[nn,]
 }
 n<-z+naninz*m
 return (n) 

 }

