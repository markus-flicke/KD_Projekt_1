CIndex <- function(data, cls) {

innerDists <- InnerClassDistances(data, cls)[[1]]
numberOfDists <- length(innerDists)
sortedAllDists <- sort(na.last=T, as.vector(dist(data)))
sMin <- sortedAllDists[1: numberOfDists]
lastInd <- length(sortedAllDists) - numberOfDists
sMax <- sortedAllDists[(lastInd +1) : length(sortedAllDists)]
nenner <- sum(sMax, na.rm = TRUE) - sum(sMin, na.rm = TRUE)

if (nenner < 10^(-7)) {
cHuber <- NaN
}
else {
cHuber <- (sum(innerDists, na.rm = TRUE) - sum(sMin, na.rm = TRUE)) / nenner
}

return(list(cHuber = cHuber, innerDists = innerDists, sMin = sMin, sMax = sMax))
}