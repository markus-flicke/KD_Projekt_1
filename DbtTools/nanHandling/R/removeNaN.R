removeNaN <- function(Data,Cls=Data*0+1){
# remove rows with NaN elements from data 
# and remove corresponding elements from cls vector
#
# INPUT
# Data        data matrix or vector variable possibly containing NaN
# Cls         vector where the same elements are to be removed
#
# OUTPUT
# cleanData   data matrix or vector cleaned of NaNs
# cleanCls    corresponing cls elements
#
# Author: RG 04/15, imported from matlab

  if(is.vector(Data)){
    keep = which(!is.nan(Data),arr.ind=TRUE)
    cleanData = Data[keep]
    cleanCls  = Cls[keep]   
  }
  
  else{
  n = nrow(Data)
  d = ncol(Data)
  
  if ((n==1) && (d > n)){ # row vector to column vector
    Data = t(Data)         
    n = nrow(Data)
    d = ncol(Data)
  }

  keep = which(rowSums(!is.nan(Data)) == d,arr.ind=TRUE)
  cleanData = Data[keep,]
  cleanCls  = Cls[keep]


  if ((d==1) && (n > d)) # result back to row vector
    cleanData = t(cleanData)
  }
  
return(list(cleanData=cleanData,cleanCls=cleanCls))
}