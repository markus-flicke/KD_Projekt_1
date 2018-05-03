ClassStdRobust <- function(Data, Cls) {
    # calulate robust standard deviation in each group of the Data
    #
    # INPUT
    # Data(d,n)         d cases,  n variables
    # Cls(d)            Cls(i) == ClusterNumber of Data(i,:)
    #
    # OUTPUT
    # UniqueClasses(AnzClass)           the  AnzClass unique classes in Cls
    # StdrobustPerClass(AnzClass,n)      StdrobustPerClass(i) is the standard deviation of the data points in UniqueClasses(i)
    #
    # OPTIONAL
    # LEAVEOUTPERCENTAGE            percentage of data that is left out default  LEAVEOUTPERCENTAGE = 20;

    uniqueClasses <- sort(na.last=T,unique(Cls))
    numberOfClasses <- length(uniqueClasses)
    stdRobust <- matrix(0, numberOfClasses, ncol(Data))

    for (i in 1: numberOfClasses ) {
        inClassInd <- which(Cls == uniqueClasses[i])
        stdRobust[i, ] <- dbt.Statistics::stdrobust(Data[inClassInd, ])
    }

    return(list(UniqueClasses = uniqueClasses, StdrobustPerClass  = stdRobust))
}
