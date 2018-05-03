ForceApproachErrorFL = function(VectorOfInputDists,VectorOfOutputDists){
# Berchnet den Force Approach Error
# 
# INPUT
# VectorOfInputDists(1:n2)            dissimilarities in Input Space between the n data points
#                                     in vector form as produced by squareform(Dists(1:n,1:n))
# VectorOfOutputDists(1:n2)           dissimilarities in Input Space between the n data points
#                                     in vector form as produced by squareform(Dists(1:n,1:n))
# 
# OUTPUT
# ForceApproachError                                   Force Approach Error
#
#
# Author: FL
#
#see: TEJADA, Eduardo; MINGHIM, Rosane; NONATO, Luis Gustavo. On improved projection techniques to support visual exploration of multi-dimensional data sets. Information Visualization, 2003, 2. Jg., Nr. 4, S. 218-231
  

  HighestInputDist = max(VectorOfInputDists)
  LowestInputDist = min(VectorOfInputDists)
  HighestOutputDist = max(VectorOfOutputDists)
  LowestOutputDist = min(VectorOfOutputDists)

  NormedInputDists = (VectorOfInputDists-LowestInputDist) / (HighestInputDist - LowestInputDist)
  NormedOutputDists = (VectorOfOutputDists-LowestOutputDist) / (HighestOutputDist - LowestOutputDist)
  
  delta = NormedInputDists - NormedOutputDists

  
  norm = 1/length(VectorOfInputDists)
  ForceApproachError = norm * sum(abs(delta))

  return(list(ForceApproachError=ForceApproachError))
}