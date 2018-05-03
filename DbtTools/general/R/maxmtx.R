maxmtx <- function(x){

  if(class(x)=="matrix"){
    maximum <- max(x)
    # Form the return vector.
    # First element max
    # Second element are the indices of the FIREST appearance
    return(c("max"=maximum,which(x==maximum,arr.ind=TRUE)[1,]))
  }
  else{
    print("The parameter has to be a matrix.")
  }
}

