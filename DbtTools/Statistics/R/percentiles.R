percentiles <- function(x, y = c(1:100)){

  ss<-sort(na.last=T,x)
  n<-length(x)
  i<-n/100
  index<-t(seq(i,n,i))
  index<-round(index)
  index<-index[1:100]
  nullInd<-which(index<1)
  index[nullInd]<-1
  p<-ss[index]
  
 return (p[y]) 

 }

