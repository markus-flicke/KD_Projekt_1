eqfloat <- function(f, g, precision=5){

  if(precision>=0){ # Only if precisiion is >=0.
    if(class(f)=="numeric" & class(g) == "numeric" & is.nan(f)==FALSE & is.nan(g)==FALSE){ # check if f and g are not nan and of class numeric
      # create a vector of precision items, item = 10
      p <- prod(rep(10,precision))
      # Multiply f and g with p and cut all other decimal places.
      return(floor(f*p)==floor(g*p))
    } # if(class(f) ==....
    else{
      print("The first 2 arguments have to be numeric and not nan.")
    }
  } # if(precision >=1)
  else{
    print("The precision must be >= 0.")
  }
}