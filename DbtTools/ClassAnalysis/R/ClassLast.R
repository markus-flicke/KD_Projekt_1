ClassLast <- function(data, cls) {
  # The last (according to the sequence of data) data set in each class
  # INPUT
  # data(d,n)         d cases,  n variables
  # cls(d)            cls(i) == ClusterNumber of data(i,:)
  # OUTPUT
  # UniqueClasses(AnzClass)      the  AnzClass unique classes in cls
  # LastPerClass(AnzClass,n)   LastPerClass(i) is the first of the data points in data which has a cls == UniqueClasses(i)
  
  uniqueClasses <- sort(na.last = T, unique(cls))
  numberOfClasses <- length(uniqueClasses)
  lastPerClass <- matrix(0, numberOfClasses, ncol(data))
  
  for (i in 1:numberOfClasses) {
    lastInClassInd <- max(which(cls == uniqueClasses[i]))
    lastPerClass[i,] <- data[lastInClassInd,]
  }
  
  return(list(UniqueClasses = uniqueClasses, LastPerClass = lastPerClass))
}
