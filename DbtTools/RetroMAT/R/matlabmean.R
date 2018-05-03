matlabmean=function(x){
# Mittelt Vector zur Zahl und Matrize zeilenweise analog zu matlab
  #author: MT  

		  requireNamespace('matrixStats')
  if(is.vector(x)){    
    mw=mean(x,na.rm = TRUE)
  }else{
    mw=as.vector(matrixStats::rowMeans(x,na.rm = TRUE))
  }
  
return(mw)  
}