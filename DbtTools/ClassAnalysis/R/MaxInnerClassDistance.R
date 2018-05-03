MaxInnerClassDistance <- function(data, cls) {
  # MaxInnerDist = MaxInnerClassDistance(data,cls);
  # the maximal euclidian distance of any of two points in the same class
  # INPUT
  # data(1:d,1:n)         d cases,  n variables
  # cls(1:d)              cls(i) == ClusterNumber of data(i,:)
  # OUTPUT
  # MaxInnerDist          the maximal euclidian distance of any of two points in the same class
  #
  # Original Author: ?
  # Editor: Felix Pape 07/16
  
  
  ## This is the syntactic equivalent of the old matlab code
  # uniqueClasses <- sort(na.last = T, unique(cls))
  # maxInnerDist <- 0
  #
  # for (i in 1:length(uniqueClasses)) {
  #   iClassInd <- which(cls == uniqueClasses[i])
  #   iData <- data[iClassInd,]
  #   maxInnerDistI <- max(dist(iData))
  #
  #   if (maxInnerDistI > maxInnerDist) {
  #     maxInnerDist <- maxInnerDistI
  #   }
  # }
  
  
  # This is much more R style code.
  classmaxdist <- function(x)
    max(dist(data[which(cls == x),]))
  maxdists = mapply(FUN = classmaxdist,
                    matrix(1:length(unique(cls))))
  maxInnerDist = max(maxdists)
  
  return(MaxInnerDist = maxInnerDist)
}