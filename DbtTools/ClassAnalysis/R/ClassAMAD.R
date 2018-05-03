ClassAMAD = function(Data, Cls) {
  # V=ClassAMAD(Data,Cls)
  # UniqueClasses=V$UniqueClasses
  # AMADPerClass=V$SPerClass
  #  Median Absolute Deviation (MAD)  in each group of the Data
  #  NaN values in Data are ignored
  #
  # INPUT
  # Data(d,n)         d cases,  n variables
  # Cls(d)            Cls(i) == ClusterNumber of Data(i,:)
  #
  # OUTPUT
  # UniqueClasses(AnzClass)      the  AnzClass unique classes in Cls
  # AMADPerClass(AnzClass,n)      MADPerClass(i) is the median Absolute Deviation (MAD) of the Data points in UniqueClasses(i)
  
  # uses nanmad(x)
  
  #author MT 06/16
  ADJUSTMENTFACTOR = 1.4826  # https://en.wikipedia.org/wiki/Median_absolute_deviation
  V = ClassMAD(Data, Cls)
  UniqueClasses = V$UniqueClasses
  MADPerClass = V$MADPerClass
  AMADPerClass = MADPerClass * ADJUSTMENTFACTOR
  
  
  return(list(UniqueClasses = UniqueClasses, AMADPerClass = AMADPerClass))
}