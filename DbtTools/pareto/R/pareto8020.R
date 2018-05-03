`pareto8020` <- function(yield,plotIt=FALSE){

# function [OptimalInd,ParetoInd, PercCumCost,PercentCumYeald] = Pareto8020(Yeald,PlotIt);
# % OptimalInd  = Pareto8020(Yeald,PlotIt);
# % find the best point for minimal number of elements to get maximal yield
# % 
# % INPUT
# % Yeald(1:n)       a maximum of this is wanted
# % OPTIONAL
# % PlotIt           =1 a plot is made, default is  PlotIt =0
# % OUTPUT
# % OptimalInd       Yeald(OptimalInd) is sorted descending in Yeald and
# %                  sum(Yeald(OptimalInd)) is maximal for minimal(length(OptimalInd)) 
# % ParetoInd        such that PercCumCost(ParetoInd),PercentCumYeald(ParetoInd) is the 80/20 point
# % PercCumCost      cumulative Costs in Percent
# % PercentCumYeald  cumulative yield in Percent

	if(sum(!is.finite(yeald))!=0){	
		print('ERROR: The data may not contain missing values.')
		return()
	}

	yeald <- as.matrix(yeald)	
	l <- nrow(yeald)
	c <- ncol(yeald)
	
	if(c>l)	
		yeald <- t(yeald)


	sortedYeald <- sort(na.last=T,yeald, decreasing=TRUE)
	sind <- order(yeald, decreasing=TRUE)
	cumYeald <- cumsum(sortedYeald)
	
	percentCumYeald <- cumYeald/cumYeald[length(cumYeald)]
	maxCost <- nrow(yeald)
	percCumCost <- as.vector(t((1:maxCost)/maxCost))
	x <- percCumCost
	y <- 1-percentCumYeald
	paretoLength <- x^2+y^2
	paretoInd <- which.min(paretoLength)
	paretoCosts <- percCumCost[paretoInd]
	paretoYeald <- percentCumYeald[paretoInd]
	optimalInd <- sind[1:paretoInd]

	if(plotIt){
		plot(lab=c(10,10,10),xaxs='i',yaxs='i',percCumCost,percentCumYeald,col='blue',type='l',xlim=c(0,1),ylim=c(0,1),main=paste('Pareto 80/20: optimal is ',round(paretoCosts*100),'% = ',paretoInd),xlab='% cost',ylab='% yeald, red = pareto 80/20 point')
		points(c(paretoCosts,paretoCosts,0),c(0,paretoYeald,1),col='red', type='l')
		
		grid(nx=10,ny=10,col='black')	
	}

 return (list(optimalInd=optimalInd,paretoInd=paretoInd,percCumCost=percCumCost,percentCumYeald=percentCumYeald) ) 

 }

