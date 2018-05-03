ztransrobust <- function(x,p=0.1){

 meanRobust<-meanrobust(x,p)
 x<-as.matrix(x)
 meanRobust<-matrix(meanRobust)
 z<- x -  t(repmat(meanRobust,1,nrow(x)))
 stdRobust<-stdrobust(x)
 if (max(stdRobust)>0){
  z[,stdRobust!=0] <- z[,stdRobust!=0]/t(repmat(stdRobust[stdRobust!=0],1,nrow(z)))
 }
 return (z) 

 }

