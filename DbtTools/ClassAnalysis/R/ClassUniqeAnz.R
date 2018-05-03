ClassUniqeAnz <- function(Data, Cls) {
  # Calulates the number of unique points in each group of the data
  # INPUT
  # Data[d,n]         d cases,  n variables
  # Cls[d]            Cls(i) == ClusterNumber of Data[i,:]
  #
  # OUTPUT
  # UniqueClasses       Number of unique classes in Cls
  # MaxPerClass         MaxPerClass[i] is the minimum of the data points in  UniqueClasses[i]
  
  UniqueClasses = unique(Cls)
  # INIT
  AnzClass = length(UniqueClasses)
  
  UniqAnzPerClass = zeros(AnzClass, 1)
  # INIT
  
  if (dim(Data)[1] != length(Cls)) {
    stop("ClassUniqeAnz: length(Data) not equal length(Cls)")
  }
  for (c in 1:AnzClass) {
    InClassInd = which(Cls == UniqueClasses[c], arr.ind = TRUE)
    if (length(InClassInd) > 0) {
      UniqAnzPerClass[c] = length(unique(Data[InClassInd]))
    }
  }
  
  return(list(UniqueClasses = UniqueClasses, UniqAnzPerClass = UniqAnzPerClass))
}