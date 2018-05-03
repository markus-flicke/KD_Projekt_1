`PDEchi2` <- function(data,df=1,paretoRadius=NaN,plotColPDE='blue',plotColChi2='green'){

# function PlotHandle = PDEchi2(Data,DF,ParetoRadius,PlotSymbolPDE,PlotSymbolChi2);
# % PlotHandle = PDEchi2(Data,DF,ParetoRadius,PlotSymbolPDE,PlotSymbolChi2);
# %
# % in /dbt/Pareto/
# %
# % DESCRIPTION
# % plot ParetoDensityEstimation (PDE) and chi2 with DF=1, empirical Mean and  Variance
# %
# % INPUT
# % Data               Vector of Data to be plotted
# % ParetoRadius       the Pareto Radius, if omitted then ParetoRadius = ParetoRad(Data);
# % PlotSymbolPDE    S Parameter for plotting PDE see Function plot,      'b-' if omitted
# % PlotSymbolChi2   S Parameter for plotting chi2 see Function plot, 'g-' if omitted
# %
# % OUTPUT
# % PlotHandle         Handle returned by function plot


	data <- as.matrix(data)
	nRow <- nrow(data)
	nCol <- ncol(data)
	
	if(nCol>nRow)
		data <- t(data)
	

	
	if(is.nan(paretoRadius)){
		paretoRadius <- ParetoRadius(data)
	}
	
	pdeVal <- PDEstimationForGauss(data,paretoRadius)
	
	m <- mean(data,na.rm=TRUE)
	s <- sd(data,na.rm=TRUE)
	
  

	chi2dist <- dchisq(pdeVal$kernels,df)
	
	plot(pdeVal$kernels,pdeVal$paretoDensity,type='l',col= plotColPDE,xlim=c(0,15),ylim=c(0,1),xlab='',ylab='',xaxs='i',yaxs='i')
	points(pdeVal$kernels,chi2dist,type='l',col=plotColChi2)




 }

