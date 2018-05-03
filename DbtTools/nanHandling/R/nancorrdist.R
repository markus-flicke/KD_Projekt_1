nancorrdist <- function(Data){

 cc<-nancor(t(Data))$out
 distanceMatrix<-1-cc
 vectorOfDistances<-unique(as.vector(distanceMatrix))
 vectorOfDistances<-vectorOfDistances[-which(vectorOfDistances==0)]
 return (list(distanceMatrix=distanceMatrix,vectorOfDistances=vectorOfDistances)) 

 }

