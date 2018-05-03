ForceApproachQ = function(VectorOfInputDists,VectorOfOutputDists){
  # Q=ForceApproachQ(VectorOfInputDists,VectorOfOutputDists)
  # Force Approach Error Q
  #
  # INPUT
  # VectorOfInputDists(1:n2)            dissimilarities in Input Space between the n data points
  #                                     in vector form as produced by squareform(Dists(1:n,1:n))
  # VectorOfOutputDists(1:n2)           dissimilarities in Input Space between the n data points
  #                                     in vector form as produced by squareform(Dists(1:n,1:n))
  # k                                   order of the Nearest Neighborhood (kNN)
  #
  # OUTPUT
  # Q                                   Forced appraoch Error
  #
  # author: MT
  #
  #see: TEJADA, Eduardo; MINGHIM, Rosane; NONATO, Luis Gustavo. On improved projection techniques to support visual exploration of multi-dimensional data sets. Information Visualization, 2003, 2. Jg., Nr. 4, S. 218-231
  
  Ni = max(VectorOfInputDists)
  Tu = max(VectorOfOutputDists)
  Input = VectorOfInputDists / Ni
  Ouput = VectorOfOutputDists / Tu
  
  deltaorig = (Input - min(VectorOfInputDists)) / (max(VectorOfInputDists) -
                                                     min(VectorOfInputDists)) - Ouput
  
  #MT:Idee statt Min%Max im Paper percentile nehmen
  #requireNamespace("dbt.Statistics")
  #imin20=dbt.Statistics::prctile(Input,20)
  imin20 = apply(Input, 2, stats::quantile, probs = 0.2)
  imax80 = apply(Input, 2, stats::quantile, probs = 0.8)
  
  omin20 = apply(Ouput, 2, stats::quantile, probs = 0.2)
  omax80 = apply(Ouput, 2, stats::quantile, probs = 0.8)
  #MT: Idee Input und Output einheitlich
  delta = (Input - imin20) / (imax80 - imin20) - (Ouput - omin20) / (omax80 -
                                                                       omin20)
  
  M = length(VectorOfInputDists)
  Q = 1 / M * sum(abs(delta))
  Qorig = 1 / M * sum(abs(deltaorig))
  return(list(
    Q = Q,
    ForceApproachError = Qorig,
    Deltas = delta,
    DeltaOriginal = deltaorig
  ))
}