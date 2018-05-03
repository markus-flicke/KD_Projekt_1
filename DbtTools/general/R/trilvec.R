trilvec <- function(matrix,k=0){
  size <- dim(matrix)
  if(k<0||k>=size[1]){
    print("Wrong use of argument k. k hast to be: 0<=k<row number.")
  }
  else{  
    utri <- lower.tri(matrix,diag=TRUE)# Upper triangular matrix with 1s and 0s.
    ind <- unname(which(utri==1,arr.ind=TRUE)) # Get indizes of upper triangular matrix.
    rm(utri)
    # Adjust the diagonal.
    ind[,1] <- ind[,1]+k
    ind <- ind[which(!ind[,1]>size[1]),]
    if(length(ind)==2){ # if ind == 2, R will return a vector instead of a matrix --> convert.
      ind <- t(ind)
    }
    # Create result
    return(matrix[ind])
  }
}