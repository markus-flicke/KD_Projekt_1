ClassPrctile <- function(Data, Cls, P) {
  # calulate percentile in each group of the data
  # INPUT
  # data(d,n)         d cases,  n variables
  # cls(d)            cls(i) == ClusterNumber of data(i,:)
  # P                 is a scalar for the percent value
  #
  # OUTPUT
  # UniqueClasses(AnzClass)      the  c unique classes in cls
  # Percentiles(AnzClass,n)       Percentiles(i,:) is the P-th percentile of
  #                               the data points in  UniqueClasses(i)
  
  uniqueClasses <- sort(na.last = T, unique(Cls))
  numberOfClasses <- length(uniqueClasses)
  percentiles <- matrix(0, numberOfClasses , ((100 / p) + 1))
  
  for (i in 1:numberOfClasses) {
    inClassInd <- which(Cls == uniqueClasses[i])
    percentiles[i,] <-
      quantile(data[inClassInd,], probs = seq(0, 1, (p / 100)))
    #percentiles[i,] <- prctile(data[inClassInd,], p)
  }
  
  return(list(UniqueClasses = uniqueClasses, Percentiles = percentiles))
}
