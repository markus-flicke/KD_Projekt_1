GroupPrctile <- function(data, cls, p) {

uniqueClasses <- sort(na.last=T,unique(cls))
numberOfClasses <- length(uniqueClasses)
percentiles <- rep(0, numberOfClasses) 

for (i in 1: numberOfClasses ) {
inClassInd <- which(cls == uniqueClasses[i])
percentiles[i] <- quantile( data[inClassInd, ], probs = (0: p)/p)
}

return(list(uniqueClasses = uniqueClasses, percentiles = percentiles))
}