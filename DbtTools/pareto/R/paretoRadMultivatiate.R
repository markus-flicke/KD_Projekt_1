`paretoRadMultivatiate` <-
function(data,maximumNrSamples=1000,expectedNumberOfClusters=6,plotDistancePercentiles=FALSE,verbose=FALSE){

# function [ParetoRadius] = ParetoRadMultivatiate(Data,MaximumNrSamples,ExpectedNumberOfClusters,PlotDistancePercentiles,Verbose);
# % [ParetoRadius] = ParetoRadMultivatiate(Data,MaximumNrSamples,ExpectedNumberOfClusters,PlotDistancePercentiles,Verbose);
# % NOTE: better call ParetoRad  with the same parameters !!!!!
# % Calculation of the ParetoRadius  
# % such that median of ParetoSpheres contains the 20.13*Inner2Inter(ExpectedNumberOfClusters)- percentile of data points
# % modyfied by the expectet number of clusters.
# % see [Ultsch03:  Optimal density estimation in data containing clusters of unknown structure] for details
# % Author: A. Ultsch 2003
# % INPUT
# % Data(1:n,1:d)             data array, cases in rows, variables in columns
# % OPTIONAL
# % MaximumNrSamples          Maximum number for which the distance calculation can be done
# %                           if omitted MaximumNrSamples =1000
# % ExpectedNumberOfClusters  the number of clusters that are expected in a data set
# %                           if omitted, the ExpectedNumberOfClusters is not known
# %                           we use the recommend ExpectedNumberOfClusters=6 see ULTSCH03
# % PlotDistancePercentiles ==1 means a plot of the percentiles of distances
# %                         is produced; may be omitted then no plottiing
# % Verbose                 ==1 give some hints about what i am doing
# % OUTPUT
# % ParetoRadius            the Pareto Radius
# 

	data <- as.matrix(data)
 
#  constants
	pu <- 0.2013 # Pareto Percentile for 1 cluster
	
	nData <- nrow(data)
	d <- ncol(data)
 

	if (maximumNrSamples >= nData){ # no sampling necessary
    	sampleData <- data
    }
	else #  sample with uniform distribution maximumNrSamples  
	{
		sampleInd <- floor(nData*c(runif(maximumNrSamples))+1)
		sampleData <- as.matrix(data[sampleInd,])
		#noDataNaNInd <- 
 		#     KeineDatenNaNInd = find(~isnan(sum(SampleData'))); % NaN in datenzeilen wegschmeissen
		#     SampleData = Data( KeineDatenNaNInd,:);
	}




	dd <- as.matrix(dist(sampleData,method='euclidean'))


	distances <- (noNaN(triuvec(dd,1)))$elements
	

	
	distancePercentiles <- percentiles(distances)
   
#    % Iteration ¸ber die mittlere Anzahl Punkte in den paretoKugeln
#    p=18;  % versuchs ertmal mit dem 18 Perzentil
	p <- 18
	radius <- distancePercentiles[p]

	
	medianHyperspheres <- rep(0,100)


	empiricalPNumbers <- inPSphere(sampleData,radius)
	medianHyperspheres[p] <- median(empiricalPNumbers)/nData
	
	

	if(verbose)
		#?????????????????????????
		
	if(medianHyperspheres[p]>pu)	
		step <- -1
	if(medianHyperspheres[p]<pu)	
		step <- 1
	




	nextStep <- step
	
	while(step==nextStep){
		p <- p+step
		radius <- distancePercentiles[p]
		empiricalPNumbers <- inPSphere(sampleData,radius)
		medianHyperspheres[p] <- median(empiricalPNumbers)/nData
		# if Verbose==1, [p MedianHyperspheres(p)]; end; % if Verbose ???????????????
		
		if(medianHyperspheres[p]>pu)	
			nextStep <- -1
		if(medianHyperspheres[p]<pu)	
			nextStep <- 1
		
		if(p>50)
		 	nextStep <- -step
		if(p<2)
			nextStep <- -step
	}
	




	minError <- min(abs(medianHyperspheres-pu))
	paretoPercentile <- which.min(abs(medianHyperspheres-pu))
	
	
#     adjust for ExpectedNumberOfClusters

	
		paretoPercentileNOCluster <- paretoPercentile
		paretoRadiusNOCluster <- distancePercentiles[paretoPercentile]
		i2i<- inner2Inter(expectedNumberOfClusters)
		if(verbose)
			print(i2i)
		paretoRadius <- paretoRadiusNOCluster *i2i







	if(plotDistancePercentiles){
		plot(1:100, distancePercentiles,type='l',col='blue', main=paste('red = ParetoRatius = ',round(paretoRadius,5),' Pareto Distance Percentile = ',paretoPercentile),xlab='Percentiles,  red = ParetoRadius, green = ParetoPercentile for 1 cluster',ylab='Distances',xaxs='i',yaxs='i')
		lines(x=c(0,paretoPercentile),y=c(paretoRadius,paretoRadius),col='red')
		lines(x=c(paretoPercentileNOCluster, paretoPercentileNOCluster),y=c(0,distancePercentiles[paretoPercentileNOCluster]),col='green')	

	}


 return (paretoRadius) 

}