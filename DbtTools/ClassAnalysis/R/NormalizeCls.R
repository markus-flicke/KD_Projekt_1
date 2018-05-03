NormalizeCls <- function(Cls) {
#E<-NormalizeCls(Cls);
#NormalizedCls    <- E$normalizedCls      #    Cls consistently recoded to positive consecutive integers
#NormalizedClasses<- E$normalizedClasses  #    the different class numbers in NormalizedCls
#UniqueCls        <- E$uniqueClasses      #    the different class numbers in Cls such that 
#AnzClasses       <- E$numberOfClasses    #    the number of different classes
# 
# Values in Cls are consistently recoded to positive consecutive integers
# INPUT
# Cls                  vector of class identifiers can be integers or
#                      NaN's, need not be consecutive nor positive
# OUTPUT list of 
# normalizedCls           Cls consistently recoded to positive consecutive integers
# normalizedClasses        the different class numbers in NormalizedCls
# uniqueClasses            the different class numbers in Cls such that 
#                           NormalizedCls(i) <-> UniqueCls(i)
# numberOfClasses           the number of different classes
  
# ALU 2014
# angepasst an Mdbt und Doku standards
  
  
  uniqueClasses <- sort(na.last = T, unique(Cls))
  numberOfClasses <- length(uniqueClasses)
  unique2Cls <- NULL #  initializing the vector
  
  for (i in 1:length(Cls)) {
    # calculating the indexes of elements of Cls in uniqueClasses
    unique2Cls <- c(unique2Cls, which(uniqueClasses == Cls[i]))
  }
  
  if (numberOfClasses > 0) {
    normalizedClasses <- c(1:numberOfClasses)
    normalizedCls <- normalizedClasses[unique2Cls]
  }
  else {
    normalizedClasses <- Cls
  }
  
  return(
    list(
      normalizedCls = normalizedCls,
      normalizedClasses = normalizedClasses,
      uniqueClasses = uniqueClasses,
      numberOfClasses = numberOfClasses
    )
  )
}