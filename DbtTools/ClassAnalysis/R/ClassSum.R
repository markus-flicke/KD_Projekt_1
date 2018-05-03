ClassSum <- function(Data, Cls) {
  # calulate Sum in each group of the Data
  # INPUT
  # Data(d,n)         d cases,  n variables
  # Cls(d)            Cls(i) == ClusterNumber of Data(i,:)
  # OUTPUT
  # UniqueClasses(AnzClass)      the  c unique classes in Cls
  # SumPerClass(AnzClass,n)      SumPerClass(i) is the Sum of the Data points in UniqueClasses(i)
  # All2UniqInd,Uniq2AllInd      aus [UniqueClasses,All2UniqInd,Uniq2AllInd] = unique(Cls);
  
  UniqueClasses = unique(Cls)
  All2UniqInd = 1:length(UniqueClasses)

  sumPerClass = c()
  for (i in UniqueClasses) {
    inClassInd = which(Cls == i, arr.ind = T)
    if(length(inClassInd)>1)
      sumPerClass = rbind(sumPerClass, colSums(Data[inClassInd,], na.rm = F))
    else{
      x=t(as.matrix(Data[inClassInd,]))
      #x[is.nan(x)]=0
      sumPerClass = rbind(sumPerClass, x)
    }
  }
  
  return(list(UniqueClasses = UniqueClasses, SumPerClass = sumPerClass))
}