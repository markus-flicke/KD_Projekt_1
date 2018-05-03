LCMC=function(InputDist,OutputDist,k){
#res=LCMC(InputDist,OutputDist,k)  
# Input
# InputDist  			Distance Matrix of Data
# OutputDist 			Distance Matrix of ProjectedPoints
# k 							An integer, giving the number of nearest neighbors to i
# Output
# MK
# MKadj  
# author: Marta Lemanczyk
  
  
  
  
  
  
  
 ################# 
  # Input:
# data: Input Data - Distance Matrix 
# i: An integer, who's number of local overlaps is sought
# k: An integer, giving the number of nearest neighbors to i
# proj: Output Data - Distance Matrix 

# Local Overlap
localNK <- function(i,k,data,proj){
  knn.D <- knneighbor(i, k, data)$nNInd
  knn.X <- knneighbor(i, k, proj)$nNInd
  lnk <- length(intersect(knn.D,knn.X))
  return(lnk)
}

# Global Overlap
NK <- function(k, data, proj){
  vek <- 1:nrow(data) 
  knns <- 0
  for (i in seq(1:length(vek))) {
    lknns <- localNK(i,k,data,proj)
    knns <- knns + lknns
  }
  nk <- 1/length(vek)*knns
  return(nk)
}
# LMCM
MK <- function(k, data, proj){
  mk <- 1/k*NK(k,data,proj)
  return(mk)
}

# LMCM adjusted for random overlap 
MKadj <- function(k,data,proj){
  mka <- MK(k,data,proj) - k/(nrow(data)-1)
  return(mka)
}

knn.vek <- function(data,proj){
  vec <- length(1:nrow(data)) 
  kn <- MKadj(1,data,proj)
  for (i in seq(2:vec)) {
    kn <- c(kn,MKadj(i,data,proj))
  }
  return(kn)
}

return(list(MK=MK(k,data=InputDist,proj=OutputDist),MKadj=MKadj(k,data=InputDist,proj=OutputDist)))
}