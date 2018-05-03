ClassMin <- function(Data, Cls) {
  # calulate minimum in each group of the Data
  # INPUT
  # Data(n,d)         n cases,  d variables
  # Cls(d)            Cls(i) == ClusterNumber of Data(i,:)
  # OUTPUT
  # UniqueClasses(AnzClass)          the  AnzClass unique classes in Cls
  # MinPerClass(1:AnzClass,1:d)      MinPerClass(i,v)    is the minimum of column v in Data for  UniqueClasses(i)
  
  # uniqueClasses <- sort(na.last = T, unique(Cls))
  # numberOfClasses <- length(uniqueClasses)
  # Size <- size(Data)
  # 
  # minPerClass <- matrix(0, numberOfClasses, Size[2])
  # 
  # if (Size[2] > 1) {
  #   for (i in 1:numberOfClasses) {
  #     C = uniqueClasses[i]
  #     inClassInd <- which(Cls == C)
  #     for (j in (1:Size[2]))
  #       minPerClass[i, j] <-
  #       min(Data[inClassInd, j], na.rm = TRUE)
  #   } # end for i
  # } else{
  #   # nur ein Vektor
  #   for (i in 1:numberOfClasses) {
  #     C = uniqueClasses[i]
  #     inClassInd <- which(Cls == C)
  #     minPerClass[i] <- min(Data[inClassInd], na.rm = TRUE)
  #   } # end for i
  # }# end if
  res = ClassApply(Data,Cls,min,na.rm=T)  
  return(list(UniqueClasses = res$UniqueClasses, MinPerClass = res$ResultPerClass))
}
