DaviesBouldin <- function(data, cls) {

mean <- ClassMean(data, cls)
uniqueClasses <- mean[[1]]
centerPerClass <- mean[[2]]
numberOfClasses <- length(uniqueClasses)
meanClass2CenterDist <- rep(0, numberOfClasses)

for (i in 1: numberOfClasses) {
classData <- data[which(cls == uniqueClasses[i]), ] 
numberInClass <- nrow(classData)
centerDists <- rbind(centerPerClass[i, ], classData)
class2CenterDist <- as.vector( dist(centerDists))[1:numberInClass]
meanClass2CenterDist[i] <- mean(class2CenterDist[1:numberInClass])
}

clusterCenterDist <- as.matrix(dist( centerPerClass, diag =TRUE, upper = TRUE))

dBArray <- matrix(0, numberOfClasses, numberOfClasses)

for ( i in 1: numberOfClasses) {
for(j in 1: numberOfClasses) {
if(i!=j)
	dBArray[i, j] <- (meanClass2CenterDist[i] + meanClass2CenterDist[j] ) /clusterCenterDist[i, j]
}
}
#print(dBArray)
dBIndex <- max( dBArray,na.rm=T ) / numberOfClasses
 
return(list(dBIndex = dBIndex, centerPerClass = centerPerClass ))
}