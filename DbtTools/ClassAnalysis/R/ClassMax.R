ClassMax <- function(Data, Cls) {
  # calulate maximum in each group of the data
  # INPUT
  # Data(n,d)         n cases,  d variables
  # Cls(d)            cls(i) == ClusterNumber of data(i,:)
  # OUTPUT
  # UniqueClasses(AnzClass)          the  AnzClass unique classes in Cls
  # MaxPerClass(1:AnzClass,1:d)      MaxPerClass(i,v)    is the maximum of column v in Data for  UniqueClasses(i)
  
  # uniqueClasses <- sort(na.last = T, unique(cls))
  # numberOfClasses <- length(uniqueClasses)
  # Size <- size(data)
  # 
  # maxPerClass <- matrix(0, numberOfClasses, Size[2])
  # 
  # if (Size[2] > 1) {
  #   for (i in 1:numberOfClasses) {
  #     C = uniqueClasses[i]
  #     inClassInd <- which(cls == C)
  #     for (j in (1:Size[2]))
  #       maxPerClass[i, j] <-
  #       max(data[inClassInd, j], na.rm = TRUE)
  #   } # end for i
  # } else{
  #   # nur ein Vektor
  #   for (i in 1:numberOfClasses) {
  #     C = uniqueClasses[i]
  #     inClassInd <- which(cls == C)
  #     maxPerClass[i] <- max(data[inClassInd], na.rm = TRUE)
  #   } # end for i
  #   
  # }# end if
  res = ClassApply(Data,Cls,max,na.rm=T)
   
  return(list(UniqueClasses = res$UniqueClasses, MaxPerClass = res$ResultPerClass))
}
