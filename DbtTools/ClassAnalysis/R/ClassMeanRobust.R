ClassMeanRobust <- function(Data, Cls, LEAVEOUTPERCENTAGE=0.1) {
  # calulate mean in each group of the Data using trimmean
  #
  # INPUT
  # Data(d,n)         d cases,  n variables
  # Cls(d)            Cls(i) == ClusterNumber of Data(i,:)
  #
  # OUTPUT
  # UniqueClasses(AnzClass)       the  AnzClass unique classes in Cls
  # MeanPerClass(AnzClass,n)      MeanPerClass(i) is the mean of the Data points in UniqueClasses(i)
  #
  # OPTIONAL
  # LEAVEOUTPERCENTAGE            percentage of data that is left out default  LEAVEOUTPERCENTAGE = 20;
  
  # UniqueClasses = unique(Cls)
  # All2UniqInd = 1:length(UniqueClasses)
  # medianPerClass = c()
  # 
  # meanPerClass = c()
  # for (i in 1:length(UniqueClasses)) {
  #   inClassInd <- which(Cls == UniqueClasses[i])
  #   meanPerClass = rbind(meanPerClass, meanrobust(Data[inClassInd,], LEAVEOUTPERCENTAGE))
  # }
  # 
  res = ClassApply(Data,Cls,dbt.Statistics::meanrobust,p = LEAVEOUTPERCENTAGE)
  return(list(UniqueClasses = res$UniqueClasses, MeanPerClass = res$ResultPerClass))
}