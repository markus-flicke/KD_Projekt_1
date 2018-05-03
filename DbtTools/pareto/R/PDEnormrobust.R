PDEnormrobust <- function(Data,paretoRadius=0,xlabel='PDE',title='PDEnormrobust',
                          PlotSymbolPDE='blue',PlotSymbolGauss= 'magenta',plotIt=TRUE,...){
# plot ParetoDensityEstimation (PDE) and Gaussian with empirical Mean and  Variance, robust estimation
# E <-  PDEnorm(Data,ParetoRadius,ylabel, title,PlotSymbolPDE,PlotSymbolGauss);
#Kernels        <- E$kernels;
#ParetoDensity  <- E$paretoDensity;
#ParetoRadius   <- E$paretoRadius;
#
# INPUT
# Data[1:n]               Vector of Data to be plotted
# 
# OPTIONAL
# ParetoRadius            the Pareto Radius, if omitted, ==0 or ==NaN then ParetoRadius = ParetoRadius(Data);
# xlabel                  label for the x-Axis of the plot 
# title                   label for the title  of the plot 
# PlotSymbolPDE           color for plotting PDE       see Function plot(...col=PlotSymbolPDE), 'blue' if omitted
# PlotSymbolGauss         color for plotting Gaussian  see Function plot(...col=PlotSymbolPDE), 'magenta' if omitted
#  
# 
# OUTPUT a list of
# kernels            the x points of the PDE function
# paretoDensity      the PDE(x)
# paretoRadius       the ParetoRadius used for the plot
  
  
# uses ParetoDensityEstimation()
# uses ParetoRadius()
  
# ALU 2014 
# angepasst an Mdbt und Doku standards
# ergaenzt um Parameter PlotSymbolPDE,PlotSymbolGauss
  


Data <- as.matrix(Data);	nRow <- nrow(Data); nCol <- ncol(Data)
if(nCol>nRow)	Data <- t(Data)
		
if(length(paretoRadius)<1 || is.nan(paretoRadius)  || (paretoRadius==0))
  paretoRadius<-ParetoRadius(Data)



pdeVal <- ParetoDensityEstimation(Data,paretoRadius) # PDEstimationForGauss(Data,paretoRadius)
m <- meanrobust(Data)
s <- stdrobust(Data)
normaldist <- dnorm(pdeVal$kernels,m,s) #the Gaussian with the empirical parametrers

	if(plotIt){
		plot(pdeVal$kernels,pdeVal$paretoDensity,col=PlotSymbolPDE,type='l',xlab=xlabel,ylab=paste('bl=PDE, mg=N(',round(m,1),',',round(s,1),')'),...)
		points(pdeVal$kernels,normaldist,col=PlotSymbolGauss,type='l')
		
	}

invisible(list(kernels=pdeVal$kernels,paretoDensity=pdeVal$paretoDensity, paretoRadius=pdeVal$paretoRadius)) 

 }# end function pdenormrobust

