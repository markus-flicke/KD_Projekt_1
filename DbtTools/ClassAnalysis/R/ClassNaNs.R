ClassNaNs <- function(Data, Cls) {
  # [UniqueClasses,NaNsPerClass] = ClassNaNs(Data,Cls);
  # calulate number of NaN in each group of the Data
  #
  # INPUT
  # Data(d,n)         d cases,  n variables
  # Cls(d)            Cls(i) == ClusterNumber of Data(i,:)
  #
  # OUTPUT
  # UniqueClasses(AnzClass)       the  AnzClass unique classes in Cls
  # NaNsPerClass(AnzClass,n)       NaNsPerClass(i) is the number of NaN of the data points in UniqueClasses(i)
  uniqueClasses <- sort(na.last = T, unique(Cls))
  numberOfClasses <- length(uniqueClasses)
  
  naNsPerClass <- matrix(0, numberOfClasses,)
  
  for (i in 1:numberOfClasses) {
    inClassInd <- which(Cls == uniqueClasses[i])
    naNsPerClass[i, ] <- sum(is.na(Data[inClassInd,]))
  }
  ## ggf folgendes machen:
  # ClassApply(Data,Cls,function(x){sum(is.na(x))})
  return(list(UniqueClasses = uniqueClasses, NaNsPerClass = naNsPerClass))
}
