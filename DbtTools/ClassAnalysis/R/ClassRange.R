ClassRange <- function(Data, Cls) {
  # calulate minimum in each group of the data
  # INPUT
  # Data(d,n)         d cases,  n variables
  # Cls(d)            cls(i) == ClusterNumber of data(i,:)
  # OUTPUT
  # ClassRanges= UniqueClasses(AnzClass)      the  c unique classes in cls
  # ClassRanges = [MinPerClass,MaxPerClass];
  
  E <- ClassMin(Data, Cls)
  MinPerClass   <- E$MinPerClass
  E <- ClassMax(Data, Cls)
  uniqueClasses <- E$UniqueClasses
  MaxPerClass   <- E$MaxPerClass
  
  return(list(
    UniqueClasses = uniqueClasses,
    RangePerClass = cbind(MinPerClass, MaxPerClass)
  ))
}
