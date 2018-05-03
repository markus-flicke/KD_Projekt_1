ClassCorrDistMean <- function(data, cls) {
  # calulate mean of correlation distance within each class
  # INPUT
  # data(d,n)         d cases,  n variables
  # cls(d)            cls(i) == ClusterNumber of data(i,:)
  # OUTPUT
  # UniqueClasses(AnzClass)            the  AnzClass unique classes in cls
  # MeanCorrDistPerClass(AnzClass)     the mean of the
  #                                    correlation distance within each class in UniqueClasses(i)
  uniqueClasses <- sort(na.last = T, unique(cls))
  numberOfClasses <- length(uniqueClasses)
  meanCorrDistPerClass <- rep(0, numberOfClasses)
  
  for (i in 1:numberOfClasses) {
    inClassInd <- which(cls == uniqueClasses[i])
    corrDist <- as.vector(dist(cor(data[inClassInd,])))
    meanCorrDistPerClass[i] <- mean(corrDist, na.rm = TRUE)
  }
  
  return(list(UniqueClasses = uniqueClasses, MeanCorrDistPerClass = meanCorrDistPerClass))
}
