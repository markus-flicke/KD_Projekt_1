DunnIndex <- function(data, cls ){
innerDist <- InnerClassDistances(data, cls)[[1]]
interDist <- InterClassDistances(data, cls)[[1]]
maxInnerDist <- max(innerDist, na.rm = TRUE)
minInterDist <- min(interDist, na.rm = TRUE)

if (maxInnerDist <- 10^(-7) ) {
dunn <- NaN }
else {
dunn <- (minInterDist / maxInnerDist)
}

return(list( dunn = dunn, maxInnerDist = maxInnerDist, minInterDist = minInterDist))
}