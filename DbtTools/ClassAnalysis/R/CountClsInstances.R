CountClsInstances <- function(cls) {

fixedCls <- NormalizeCls(cls)
cls <- fixedCls[[1]]
uniqueClasses <- fixedCls[[3]]
numberOfClasses <- length(uniqueClasses)
classCount <- rep(0, numberOfClasses)
classFrequency <- rep(0, numberOfClasses)
# counting the occurances of each unique class in cls
for (i in 1: numberOfClasses) {
classCount[i] <- length( which( cls == uniqueClasses[i]))  
}

# dividing the classCount by the number of elements in cls, thus getting the class # percentages.
for (i in 1: numberOfClasses) { classFrequency[i] <- (classCount[i] / length(cls))*100 
} 

return(list(classCount = classCount, classFrequency = classFrequency, uniqueClasses = uniqueClasses))
}