MCE  = function(DataOrDistances,OutputDimension=2,Centring=T, EmbeddingMethod = "SVD"){
  # wrapper for internal function mce by its original inventing authors. 
  # ProjectedPoints <- MCE(Data, OutputDimension)
  # INPUT
  # DataOrDistances    Data or pairwise Distancematrix. If symmetric a Distancematrix is assumed.
  # Centring             if True, the dataset is centered
  # EmbeddingMethod   Method for Embedding the kernel onto OutputDimensions. "SVD" or "MDS"
  # OUTPUT
  # ProjectedPoints
  # src: https://sites.google.com/site/carlovittoriocannistraci/5-datasets-and-matlab-code/minimum-curvilinearity-ii-april-2012
  
  if(isSymmetric.matrix(DataOrDistances)){
    x = DataOrDistances
  }
  else{
    x = as.matrix(dist(DataOrDistances))
  }
  
  n = OutputDimension
  centring = Centring
  
    # This code is performs just MCE and ncMCE (both SVD-based)
    
    #Given a distance or correlation matrix x, it performs Minimum Curvilinear 
    #Embedding (MCE) or non-centred MCE (ncMCE) (coded 24-March-2013 by 
    #Gregorio Alanis-Lobato and checked by Carlo Vittorio Cannistraci)
    
    #INPUT
    #   x => Distance (example: Euclidean) or distance-adjusted correlation matrix (example: x = 1 - Pearson_correlation)
    #   n => Dimension into which the data is to be embedded
    #   centring => 'yes' if x should be centred or 'no' if not
    #OUTPUT
    #   s => Coordinates of the samples being embedded into the reduced n-dimesional space
    
    # src: https://sites.google.com/site/carlovittoriocannistraci/5-datasets-and-matlab-code/minimum-curvilinearity-ii-april-2012
    
    #Make sure the required library 'igraph' is installed and load it
    if(!require("igraph")) stop("MCE: igraph has to be installed")
    
    #Make sure the matrix is symmetric
    x <- pmax(x, t(x));
    
    
    #Create a graph object out of the adjacency matrix x
    g <- graph.adjacency(x, mode = "undirected", weighted = TRUE);
    
    #MC-kernel computation
    mst <- minimum.spanning.tree(g);
    kernel <- shortest.paths(mst);
    
    #Kernel centring
    if(centring){
      N <- nrow(kernel);
      J <- diag(N) - (1/N)*matrix(1, N, N); #Form the centring matrix J
      kernel <- (-0.5)*(J %*% kernel^2 %*% J);
    }
    
    if(EmbeddingMethod == "SVD"){
      s <- SVDEmbedding(kernel, n)
    }
    else if(EmbeddingMethod == "MDS"){
      s <- MDS(kernel, OutputDimension = n)$ProjectedPoints
    }
    
    return(s)
}