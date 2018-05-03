MinInterClassDistance <- function(data, cls) {
  # the minimal euclidian distance of any of two points in two different classes
  # INPUT
  # Data(1:d,1:n)                   d cases,  n variables
  # Cls(1:d)                        Cls(i) == ClusterNumber of Data(i,:)
  # OUTPUT
  # MinInterDist                    the minimal distance distance of any of two points in two different classes
  # ShortestInterDistances(1:d,1:d) ShortestInterDistances(a,b) is the minimal distance between Classes Cls(a) and Cls(b)
  
  icd <- InterClassDistances(data, cls)
  classNrA <- icd[[2]]
  classNrB <- icd[[3]]
  interClassDistances <- icd[[1]]
  uniqueClasses <- sort(na.last = T, unique(cls))
  numberOfClasses <- length(uniqueClasses)
  shortestInterClassDistances <-
    matrix(0, numberOfClasses, numberOfClasses)
  
  for (i in 1:numberOfClasses) {
    classAInd <- which(classNrA == i)
    
    for (j in i:numberOfClasses) {
      if (j != i) {
        classBInd <- which(classNrB[classAInd] == j)
        classABDistances <- interClassDistances[classBInd]
        minABDistance <- min(classABDistances)
        shortestInterClassDistances[i, j] <- minABDistance
      }
    }
  }
  minInterDist <-
    min(shortestInterClassDistances[upper.tri(shortestInterClassDistances)])
  shortestInterClassDistances <-
    shortestInterClassDistances + t(shortestInterClassDistances)
  return(
    list(
      MinInterDist = minInterDist,
      ShortestInterClassDistances = shortestInterClassDistances
    )
  )
}