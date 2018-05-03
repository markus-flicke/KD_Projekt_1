`paretoRad` <-
function(data,maximumNrSamples=1000,expectedNumberOfClusters=6,plotDistancePercentiles=FALSE,verbose=FALSE){

# function [ParetoRadius] = ParetoRad(Data,MaximumNrSamples,ExpectedNumberOfClusters,PlotDistancePercentiles,Verbose);
# % [ParetoRadius] = ParetoRad(Data,MaximumNrSamples,ExpectedNumberOfClusters,PlotDistancePercentiles,Verbose);
# % Calculation of the ParetoRadius  
# % uses  ParetoRadiusfuerGMM   for 1D, 
# % uses  ParetoRadMultivatiate for higher dimensions
# % such that median of ParetoSpheres contains an information optimal number of data points (20.13%)
# % modified by the expected number of clusters.
# % see [Ultsch03] A. Ultsch:"Optimal density estimation in data containing clusters of unknown structure" for details
# % NOTE: in the case of univariate data the radius is determined using 
# % ParetoRadiusfuerGMM(Data,MaximumNrSamples,PlotDistancePercentiles);
# % if this is unwanted, use  ParetoRadMultivatiate instead
# %
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
# %
# % Author: A. Ultsch 2003/2004
# 
# % default values for optiopnal Parameters

	if(expectedNumberOfClusters<=0)
		expectedNumberOfClusters <- 6

	data <- as.matrix(data)
	nrOfCases <- nrow(data)
	nrOfDimensions <- ncol(data)
	
#  determine, whether to caclulate univatiate or multivariate ParetoRadius
	if(nrOfDimensions<2)
		paretoRadius <- ParetoRadius(data,maximumNrSamples,plotDistancePercentiles)
	else 
		paretoRadius <- paretoRadMultivatiate(data,maximumNrSamples,expectedNumberOfClusters,plotDistancePercentiles,verbose)
		

 	return (paretoRadius) 

 }

