ClassFirst <- function(data, cls) {
  # The first (according to the sequence of data) data set in each class
  # INPUT
  # data(d,n)         d cases,  n variables
  # cls(d)            cls(i) == ClusterNumber of data(i,:)
  # OUTPUT
  # UniqueClasses(AnzClass)         the  AnzClass unique classes in cls
  # FirstPerClass(AnzClass,n)       FirstPerClass(i) is the first of the data points in data which has has a cls == UniqueClasses(i)
  # FirstInClassInd(1:AnzClass,1))  Indices of the fist element in Cls
  
  uniqueClasses <- sort(na.last = T, unique(cls))
  numberOfClasses <- length(uniqueClasses)
  firstPerClass <- matrix(0, numberOfClasses, ncol(data))
  
  for (i in 1:numberOfClasses) {
    firstInClassInd <- min(which(cls == uniqueClasses[i]))
    firstPerClass[i, ] <- data[firstInClassInd,]
  }
  
  return(list(UniqueClasses = uniqueClasses, FirstPerClass = firstPerClass))
}
