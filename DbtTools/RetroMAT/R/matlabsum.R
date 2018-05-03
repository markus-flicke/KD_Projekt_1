matlabsum=function(x){
# Summiert Vector zur Zahl und Matrize zeilenweise analog zu matlab
  #author: MT  
	  requireNamespace('matrixStats')
  if(is.vector(x)){    
    summe=sum(x,na.rm = TRUE)
  }else{
    summe=as.vector(matrixStats::rowSums(x,na.rm = TRUE))
  }
  
return(summe)  
}