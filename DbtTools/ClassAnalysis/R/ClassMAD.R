ClassMAD = function(Data, Cls) {
  #  V=ClassMAD(Data,Cls)
  #  UniqueClasses=V$UniqueClasses
  #  MADPerClass=V$MADPerClass
  #  Median Absolute Deviation (MAD)  in each group of the Data
  #  NaN values in Data are ignored
  #
  # INPUT
  # Data(d,n)         d cases,  n variables
  # Cls(d)            Cls(i) == ClusterNumber of Data(i,:)
  #
  # OUTPUT
  # UniqueClasses(AnzClass)      the  AnzClass unique classes in Cls
  # MADPerClass(AnzClass,n)      MADPerClass(i) is the median Absolute Deviation (MAD) of the Data points in UniqueClasses(i)
  
  # uses nanmad(x)
  
  UniqueClasses = unique(Cls)
  All2UniqInd = 1:length(UniqueClasses)
  # Uniq2AllInd=V$Uniq2AllInd
  
  AnzClass = length(UniqueClasses)
  MADPerClass = c()
  for (c in 1:AnzClass) {
    InClassInd = which(Cls == UniqueClasses[c], arr.ind = T)
    MADPerClass = cbind(MADPerClass, nanmad(Data[InClassInd, ]))
  }  # for c
  return(list(
    UniqueClasses = UniqueClasses,
    MADPerClass = t(MADPerClass)
  ))
} 