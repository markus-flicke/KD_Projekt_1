SVDEmbedding = function(DataOrDistances, OutputDimension){
  # Does SVD Embedding based on snippet from MCE Code.
  # ProjectedPoints <- SVDEmbedding(DataOrDistances, OutputDimension)
  # INPUT
  # DataOrDistances   Data or pairwise distances. If symmetric, a distance matrix is assumed.
  # OutputDimension  Dimensions that Data will be embedded on
  # OUTPUT
  # ProjectedPoints
  
  if(!is.matrix(DataOrDistances)) stop("SVDEmbedding: DataOrDistances has to be a matrix.")
  
  distances = 
  # if matrix is not symmetric => dataset => calculate distances
  if(!isSymmetric.matrix(DataOrDistances)){
    distances = as.matrix(dist(DataOrDistances))
  }
  else{
    distances = DataOrDistances
  }
  
  res <- svd(distances);
  L <- diag(res$d);
  V <- res$v;
  
  sqrtL <- sqrt(L[1:OutputDimension, 1:OutputDimension]);
  V <- V[, 1:OutputDimension];
  
  s <- t(sqrtL %*% t(V));
  
  return(s)
}