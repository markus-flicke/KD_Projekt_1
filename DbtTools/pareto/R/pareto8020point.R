`pareto8020point` <- function(percentCumCost,percentCumYield,plotIt=FALSE){

# function [ParetoInd] = Pareto8020point(PercCumCost,PercentCumYield,PlotIt);
# % OptimalInd  = Pareto8020(Yeald,PlotIt);
# % find the Pareto point for given PercCumCost,PercentCumYield
# % 
# % INPUT
# % PercCumCost(1:n),PercentCumYield(1:n) define the Pareto Plot ( sorted! and in [0 1] range
# % OPTIONAL
# % PlotIt           =1 a plot is made, default is  PlotIt =0
# % OUTPUT
# % ParetoInd       PercCumCost(ParetoInd),PercentCumYield(ParetoInd) is the Pareto Point
# %                  i.e the point closest to (0,1)

	percentCumCost <- as.matrix(percentCumCost)
	rC <- nrow(percentCumCost)
	cC <- ncol(percentCumCost)
	if(cC>rC) percentCumCost <- t(percentCumCost)
	
	percentCumYield <- as.matrix(percentCumYield)
	rC <- nrow(percentCumYield)
	cC <- ncol(percentCumYield)
	
	if(cC>rC) percentCumYield <- t(percentCumYield)
	
    x <- percentCumCost
    y <- 1-percentCumYield
   
    paretoLength <- x^2 + y^2
    
    paretoInd <- which.min(paretoLength)
    paretoCosts <- percentCumCost[paretoInd]
	paretoYeald <- percentCumYield[paretoInd]
	
	if(plotIt){
		plot(lab=c(10,10,10),xaxs='i',yaxs='i',percentCumCost,percentCumYield,col='blue',type='l',xlim=c(0,1),ylim=c(0,1),main=paste('Pareto 80/20: optimal is ',round(paretoCosts*100),'% = ',paretoInd),xlab='% cost',ylab='% yeald, red = pareto 80/20 point')
		points(c(paretoCosts,paretoCosts,0),c(0,paretoYeald,1),col='red', type='l')
		grid(nx=10,ny=10,col='black')	
	}

 return (paretoInd) 

 }

