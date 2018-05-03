zinvlog <- function(l,b=0){
  if(nargs()>1){ # Is b specified?
    if(b>=0){ # Only use base >= 0.
      x = l
      # Find index to to perform zinvlog.
      ind = which(l>1)
      x[ind] = b^(l[ind]-1)
      return(x)
    }
    else{
      print("The second argument musst be >= 0.")
    }
  }
  else{
    x = l
    ind = which(l>1)
    x[ind] = exp(l[ind]-1)
    return(x)
  }
}

