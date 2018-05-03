ClassTrimMean <- function(data, cls, percent = 10) {
  # calulate the trimmed mean in each group of the data
  # excluding the highest and lowest percent/2 in each group of the observations.
  # INPUT
  # data(d,n)         d cases,  n variables
  # cls(d)            cls(i) == ClusterNumber of data(i,:)
  # OPTIONAL
  # percent          the highest and lowest percent/2 are excluded to calculate mean in each group of the observations
  #                  if not given: percent=10;
  # OUTPUT
  # UniqueClasses(AnzClass)       the  AnzClass unique classes in cls
  # TrimMeanPerClass(AnzClass,n)      TrimMeanPerClass(i) is the TrimMean of the data points in UniqueClasses(i)
  
  uniqueClasses <- sort(na.last = T, unique(cls))
  numberOfClasses <- length(uniqueClasses)
  percent <- (percent / 100) / 2
  trimMeanPerClass <- matrix(0, numberOfClasses,  ncol(data))
  
  for (i in 1:numberOfClasses) {
    inClassInd <- which(cls == uniqueClasses[i])
    #trimMeanPerClass[i, ] <- mean( data[inClassInd, ], trim = percent, na.rm = TRUE)
    trimMeanPerClass[i, ] <-
      apply(
        X = data[inClassInd, ],
        FUN = mean,
        MARGIN = 2,
        trim = percent,
        na.rm = T
      )
  }
  
  return(list(UniqueClasses = uniqueClasses, TrimMeanPerClass = trimMeanPerClass))
}
