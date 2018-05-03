InnerClassDistances <- function(Data, Cls) {
# V = InnerClassDistances(Data,Cls)
# all Inner Class Distances
# INPUT
# Data(1:n,1:d)          n cases,  d variables
# Cls(1:n)               Cls(i) == ClusterNumber of Data(i,:)
# OUTPUT
# InnerDist(1:di)        Vector of all inner class Distances
# InnerClassNumbers(1:di)     the corresponding Cluster numbers
# author : ?
# 1. Editor: MT 06/2015
uniqueClasses <- sort(na.last=T,unique(Cls))
numberOfClasses <- length(uniqueClasses)
innerDist <- NULL
innerClassNumbers <- NULL

for (i in 1: numberOfClasses) {
inClassInd <- which(Cls == uniqueClasses[i])
inClassData <- Data[inClassInd, ] #alle Daten des Clusters c (== ci)
innerDistI <-as.vector(dist(inClassData)) # alle Distanzen des clusters c
innerDist <- c(innerDist, innerDistI)	# anfuegen
innerClassNumbers <- c(innerClassNumbers, rep(uniqueClasses[i], length(innerDistI)))
}

return(list(InnerDist = innerDist, InnerClassNumbers = innerClassNumbers))
}
  