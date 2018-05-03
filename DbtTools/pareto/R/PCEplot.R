`PCEplot` <- function(data,paretoRadius=0,plot=TRUE,title=''){

# function [Kernels,IntegralParetoDensity,ParetoRadius] = PCEplot(Data,ParetoRadius,PlotSymbolPDE);
# % [Kernels,ParetoDensity,ParetoRadius] = PCEplot(Data,ParetoRadius,PlotSymbolPDE);
# % Plots the numerically integration of the ParetoDensityEstimation 
# %
# % INPUT
# % Data               Vector of Data to be plotted
# % ParetoRadius       the Pareto Radius, if omitted or ==0 then ParetoRadius = ParetoRad(Data);
# % PlotSymbolPDE      Parameter for plotting PDE see Function plot,      'b-' if omitted
# %
# % OUTPUT
# % Kernels                 data values at which IntegralParetoDensity is measured 
# % IntegralParetoDensity   Approximation of cumulative density distribution at x=Kernels 
# % ParetoRadius            der Pareto Radius
# %
# % Author: ALU 2004
# % 
# % in /dbt/Pareto/
#

	data <- as.matrix(data)
	nRow <- nrow(data)
	nCol <- ncol(data)
	
	if(nCol>nRow)
		data <- t(data)

	if(paretoRadius<=0)
		paretoRadius <-ParetoRadius(data)


	pceVal <- PCE(data,paretoRadius) 


	if(plot){
  plot(pceVal$kernels,pceVal$integralParetoDensity,ylim=c(min(pceVal$integralParetoDensity,na.rm=TRUE),max(pceVal$integralParetoDensity,na.rm=TRUE)),xlim=c(min(pceVal$kernels,na.rm=TRUE),max(pceVal$kernels,na.rm=TRUE)),typ='l',col="blue",main=title,xlab='Data',ylab='Integral on PDE') 
}

return(list(kernels=pceVal$kernels,integralParetoDensity=pceVal$integralParetoDensity,paretoRadius=pceVal$paretoRadius))

 }

