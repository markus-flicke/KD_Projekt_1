matlabmax=function(x,ColsOrRows=1,vergleich=NULL){
  # Bestimmt das MAximum eines Vector zur Zahl und einer Matrize zeilenweise analog zu matlab
  #author: MT
  requireNamespace('matrixStats')

  
  if(is.vector(x)){    
    maxi=max(x,na.rm = TRUE)
    if(!is.null(vergleich)){maxi=max(c(maxi,vergleich))}
  }else{
    if(ColsOrRows==1){
      maxi=matrixStats::rowMaxs(x,na.rm = TRUE)
    
      if(!is.null(vergleich)){maxi=matrixStats::rowMaxs(cbind(mini,vergleich))}
    }else{
      maxi=matrixStats::colMaxs(x,na.rm = TRUE)
      
      if(!is.null(vergleich)){maxi=matrixStats::colMaxs(cbind(mini,vergleich))}
    }
  }
  
  return(maxi)  
}



