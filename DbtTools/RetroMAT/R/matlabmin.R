matlabmin=function(x,ColsOrRows=1,vergleich=NULL){
  # Bestimmt das Minimum eines Vector zur Zahl und einer Matrize zeilenweise analog zu matlab
  #author: MT
  requireNamespace('matrixStats')
  if(is.vector(x)){    
    mini=min(x,na.rm = TRUE)
    if(!is.null(vergleich)){mini=min(c(mini,vergleich))}
  }else{
    if(ColsOrRows==1){
       mini=matrixStats::rowMins(x,na.rm = TRUE)
    
      if(!is.null(vergleich)){mini=matrixStats::rowMins(cbind(mini,vergleich))}
    }else{
      mini=matrixStats::colMins(x,na.rm = TRUE)
      
      if(!is.null(vergleich)){mini=matrixStats::colMins(cbind(mini,vergleich))}
    }
  }
  
  return(mini)  
}


