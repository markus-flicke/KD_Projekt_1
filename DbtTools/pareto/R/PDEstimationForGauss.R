PDEstimationForGauss <- function(Data,paretoRadius=0,kernels=NaN){

#function [Kernels,ParetoDensity,ParetoRadius] = PDEstimationForGauss(Data,ParetoRadius,Kernels);
#% [Kernels,ParetoDensity,ParetoRadius] = PDEstimationFuerGauss(Data,ParetoRadius,Kernels);
#% calculate the ParetoDensity for an one dimensional distribution 
#% INPUT
#% DataNaN                 die eindimensional verteilten Daten mit eventuellen NaN
#% ParetoRadius            der Pareto Radius, wenn nicht angegeben, wird er berechnet
#% Kernels                 Data values at which ParetoDensity is measured, optional
#%
#% OUTPUT
#% Kernels                 Data values at which ParetoDensity is measured , use plot(Kernels,ParetoDensity) for display
#% ParetoDensity           die mit dem ParatoRadius ermittelte Dichte
#% ParetoRadius            der Pareto Radius
#%
#% in /dbt/Pareto/
#% AUTOR : ALU

#% der ParatoRadius wir mit max. 1000 datenpunkten bestimmt

# Remove NaN bevore flight

#require(caTools)

	Data <- Data[!is.nan(Data)]
	Data <- as.matrix(Data)
	
	z <- dim(Data)[1]
	s <- dim(Data)[2]

	
	
	if (s>z){ 
	
		Data <- t(Data)
  	 	z <- dim(Data)[1]
		s <- dim(Data)[2]
	
   }
    
   
   if(s>1){
print("'ParetoProbDens: Dimension of Data > 1")
  		return
   	} 
    
    if(paretoRadius==0){
    	paretoRadius<-ParetoRadius(Data)
    }


	if (is.nan(kernels)){
		nBins <- OptNrOfBins(Data)
		minD <- min(Data)	
		maxD <- max(Data)
		binWidth <- (maxD-minD)/(nBins*3)
		kernels <- seq(minD+binWidth/2,maxD-binWidth/2,len=nBins*3-1)
		
	}



# remove NaN from Kernels & Sort
	kernels <- kernels[!is.nan(kernels)]
	kernels <- as.matrix(kernels)
	
	kernels <- sort(na.last=T,kernels)
	nKernels <- length(kernels)
	


	paretoDensity <- seq(0,(nKernels-1))


	upperB <- kernels + paretoRadius
	lowerB <- kernels - paretoRadius

	dd <- sort(na.last=T,Data)


	for(i in 1:nKernels){
		dd <- dd[dd>=lowerB[i]]
		isInPareto <- dd<=upperB[i]
		paretoDensity[i] <- sum(isInPareto)
				
	}
	
	#idx = 2:length(kernels)
	# Hier sollte Integriert werden! wird es?
	area <- trapz(kernels,paretoDensity)
	#(as.double((kernels[idx]-kernels[idx-1])%*%(paretoDensity[idx]+paretoDensity[idx-1]))/2)
	area <- abs(area)
	
	if(area < 0.0000000001){
		paretoDensity <- paretoDensity*0
	}else {
		paretoDensity <- paretoDensity/area
	}



return(list(kernels=kernels,paretoDensity=paretoDensity,paretoRadius=paretoRadius))


}
