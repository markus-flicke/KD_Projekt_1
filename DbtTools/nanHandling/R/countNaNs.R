countNaNs <-  function(x, d=1) {
# Number of values equal to NaN along dimension d
# If d is omitted, it defaults to 1 (column-wise).
#
# INPUT
# x         data matrix
# d         dimension: 1=column-wise 2=row-wise
#
# OUPUT
# n         vector with number of values NaN

# ALU 2014
  
if  (length(dim(x)) ==2) { # es ist wirklich eine Matrix 
    SpaltenSum <- apply(x, 2, function(x)  sum(is.na(x)))
    ZeilenSum  <- apply(x, 1, function(x)  sum(is.na(x)))
  }else{# (length(dim(Data)  ungleich 2) 
    SpaltenSum <- NaN;
    ZeilenSum <- sum(is.na(x));    
  } #     if  (length(dim(Data) ==2) 
  
if (d==1){ # column-wise
    return(SpaltenSum)
  }else{  # row-wise
    return(ZeilenSum)
} # end if (d==1)

} # end function countNaNs

