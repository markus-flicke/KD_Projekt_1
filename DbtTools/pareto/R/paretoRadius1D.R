`paretoRadius1D` <-
function(data,maximumNrSamples=1000,plotDistancePercentiles=FALSE){
# MT: Dient zur schneller Abschätzung des Paretoradiuses, normalerweise immer ParetoRadius verwenden
# function [ParetoRadius] = ParetoRadius1D(Data,MaximumNrSamples,PlotDistancePercentiles);
# % [ParetoRadius] = ParetoRadius1D(Data,MaximumNrSamples,PlotDistancePercentiles);
# %
# % in /dbt/Pareto/
# %
# % DESCRIPTION
# % Fast Pareto Radius estimation optimal for 1D Gaussian mixtures: 
# % 18th percentile of pairwise Euclidian distances.
# %
# % INPUT
# % Data(1:n,1:d)           data array, cases in rows, variables in columns
# % MaximumNrSamples        Maximum number for which the distance calculation can be done
# %                         if omitted MaximumNrSamples =1000
# % PlotDistancePercentiles ==1 means a plot of the percentiles of distances
# %                         is produced; may be omitted then no plottiing
# %
# % OUTPUT
# % ParetoRadius            the Pareto Radius


	data <- as.matrix(data)
	
	paretoPercentile <- 18/100 # see Ultsch2001
	
	nData <- nrow(data)


	if (maximumNrSamples >= nData){ # no sampling necessary
    	sampleData <- data
    }
	else #  sample with uniform distribution MaximumNrSamples  
	{
		
		sampleData <- as.matrix(data[floor(nData*matrix(runif(1* maximumNrSamples),1))+1,])
 
	}
	
	pd <- c(dist(sampleData,method='euclidean'))


 
#  selction of ParetoRadius
	paretoRadius <- prctile(pd,paretoPercentile)

	if(paretoRadius==0){
		pct <- prctile(pd,1:100)
		 paretoRadius <-  min(pzt[pzt>0]) # take the smallest nonzero
	}

	if(plotDistancePercentiles){
		pct <- prctile(pd,(1:100)/100)
		plot(1:100,pct,type='l',col='blue', main='red = ParetoRatius',xlab='Percentiles',ylab='Distances')
		lines(x=c(paretoPercentile*100, paretoPercentile*100),y=c(0,paretoRadius),col='red')
	}

 return (paretoRadius) 

 }

