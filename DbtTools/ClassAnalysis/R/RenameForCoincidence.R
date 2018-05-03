RenameForCoincidence <- function(givenCls, standardCls)  {
  #requireRpackage('pracma')
  uniqueClasses <- sort(na.last = T, unique(standardCls))
  requireNamespace('pracma')
  allPossiblePermutations <- pracma::perms(uniqueClasses)
  nrOfPermutations <- nrow(allPossiblePermutations)
  nrOfStdClasses <- ncol(allPossiblePermutations)
  givenClasses <- sort(na.last = T, unique(givenCls))
  nrOfGivenClasses <- length(givenClasses)
  renamedCls <- givenCls
  bestAccuracy <- 0
  #For every permutation
  for (i in 1:nrOfPermutations) {
    #set ground truth
    tryRenameCls <- givenCls
    
    #set a permutation of cls to be inspected
    newClassNames <- c(1:nrOfGivenClasses)
    newClassNames[1:nrOfStdClasses] <- allPossiblePermutations[i,]
    
    for (j in 1:nrOfGivenClasses) { #for every point
      #search
      tryRenameCls[which(givenCls == givenClasses[j])] <- newClassNames[j]
    }
    #true positives
    accuracy <- sum(tryRenameCls == standardCls)
    
    if (accuracy > bestAccuracy) {
      renamedCls <- tryRenameCls
      bestAccuracy <- accuracy
    }
  }
  
  bestAccuracy <- bestAccuracy / length(standardCls)
  return(list(renamedCls = renamedCls, bestAccuracy = bestAccuracy))
}