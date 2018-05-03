InterClassDistances <- function(Data, Cls) {
# V = InterClassDistances(Data,Cls)
# all Inter Class Distances
# INPUT
# Data(1:n,1:d)          n cases,  d variables
# Cls(1:n)               Cls(i) == ClusterNumber of Data(i,:)
# OUTPUT
# InterDist(1:di)         Vector of all inner class Distances
# ClassNrA,ClassNrB      the corresponding Cluster numbers
#                        such that   InterDist(i) is a distance 
#													between Clusters  ClassNrA(i) andClassNrB(i)
# author : ?
# 1. Editor: MT 06/20
cc <- ClassCount(Cls)[[2]]
uniqueClasses <- sort(na.last=T,unique(Cls))
numberOfClasses <- length(uniqueClasses)
interDists <- NULL
classNrA <- NULL
classNrB <- NULL
for (i in 1: numberOfClasses) {
classA <- uniqueClasses[i]
numberInA <- cc[i]
for (j in i : numberOfClasses) {
if(i != j) {
classB <- uniqueClasses[j]
numberInB <- cc[j]
inA <- which(Cls == classA)
inB <- which(Cls == classB)
inAThenBInd <- c(inA, inB)  # erst alle A dann alle B
inABData <- Data[inAThenBInd, ]
inABDists <- as.matrix(dist(inABData, method = "euclidean", diag = TRUE, upper = TRUE)) # alle Distanzen A-A 
aBDists <- inABDists[1: numberInA, (numberInA +1) : ncol(inABDists)] # Distanzen A-B abgreifen
aBDists <- as.vector(aBDists)
interDists <- c(interDists, aBDists)
classNrA <- c(classNrA, rep(classA, length(aBDists)))
classNrB <- c(classNrB, rep(classB, length(aBDists)))
}
}
}

return( list(InterDist = interDists, ClassNrA = classNrA, ClassNrB = classNrB))
}