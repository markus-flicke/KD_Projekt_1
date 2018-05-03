errorbarh <-
function(MeanData, LowBound, HighBound=LowBound, Labels=NULL, MeanColor="blue", LineColor="blue"){
                                                                                   
 #min value of the data set 
 dMin <- min(MeanData)
 #max value of the data set
 dMax <- max(MeanData)
 #lower limit for x axis
 xMin <- dMin - 3*LowBound[1]
 #higher limit for x axis            
 xMax <- dMax + 3*HighBound[length(HighBound)]
 
 plot(MeanData, c(1:length(MeanData)), pch=8, col=MeanColor, xlim=c(xMin, xMax), xlab="Data", ylab="ecdf", na.rm=TRUE)
 
 for(i in 1:length(MeanData)){
    yCor <- c(i,i)
    xLow <- MeanData[i]-LowBound[i]
    xHigh <- MeanData[i]+HighBound[i]
    xCor <- c(xLow, xHigh)
    lines(x=xCor, y=yCor, col=LineColor)
 }
 }

