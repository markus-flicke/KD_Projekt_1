zlog <- function(x,b=0){
  if(nargs()>1){ # Is b specified?
    if(b>=0){ # Only use base >= 0.
      # Find index to to perform zlog.
      ind = which(x>1)
      x[ind] = log(x[ind])/log(b)+1
      return(x)
    }
    else{
      print("The second argument musst be >= 0.")
    }
  }
  else{
    ind = which(x>1)
    x[ind] = log(x[ind])+1;
    return(x)
  }
}