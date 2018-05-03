noNaN <- function(data){
  #
  # return elements of data not equal to NaN/NA or Inf
  #
  # INPUT
  # data        vector or matrix of data
  #
  # OUTPUT
  # Numbers     all elements in A that are not NaN or Inf
  # NoNaNInd    Index such that Numbers =  data[NoNaNInd]
  # AnzNumbers  ==length(NoNaNInd)
  
  nonanind = which(is.finite(data))
  numbers = data[nonanind]
  
  return(list(NoNaNInd = nonanind, Numbers = numbers, AnzNumbers = length(nonanind)))
}
