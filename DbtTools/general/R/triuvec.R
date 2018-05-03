triuvec <- function(matrix,k=0){
  size <- dim(matrix)
  if(k<0||k>=size[2]){
    print("Wrong use of argument k. k hast to be: 0<=k<column number.")  
  }
  else{  
    utri <- upper.tri(matrix,diag=TRUE)# Upper triangular matrix with 1s and 0s.
    ind <- unname(which(utri==1,arr.ind=TRUE)) # Get indizes of upper triangular matrix.
    rm(utri)
    # Adjust the diagonal.
    ind[,2] <- ind[,2]+k
    ind <- ind[which(!ind[,2]>size[2]),]
    if(length(ind)==2){ # if ind == 2, R will return a vector instead of a matrix --> convert.
      ind <- t(ind)
    }
    # Create result
    return(matrix[ind])
  }
}