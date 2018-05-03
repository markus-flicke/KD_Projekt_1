`PDECenters` <-
function(data, paretoRadius=NaN){

# function [CentersInd,AnzInKugel] = PDECenters(data, ParetoRadius);
# % DensityCentersInd = PDEPrototypes(data);
# % calculation of most dense  points in a data set
# % makes use of the Pareto Density estimation
# % INPUT
# % data      the data each case in a row
# % OPTIONAL
# % ParetoRadius  the Paratoradius, if omitted  ParetoRadius = ParetoRad(data);
# % OUTPUT
# % CentersInd   a vector of indices into data such that data(DensityCentersInd,:) are
# %                     the most dense centers 
# % AnzInKugel  die PDE an jeder Zeile in data
# %
# % ALGORITHMUS: weggelassen werden
# % 1. Daten in den duensten Regionen PDE < DichteSchwelle = 0.2013 *prctile(AnzInKugel,95)
# % 2. Daten mit PDE < 2* DichteSchwelle deren lokale Dichte‰nderung DP kleiner als  die Dichtefluktuation DP ist.
# % DP = Differenz der PDE zum n‰chten Nachbarn mit Abstand >0 geteilt durch die Entfernung zu diesem Nachbarn .
# % Die Dichtefuktuation ist 0.5 * s(lokale Dichte‰nderung) , wobei  s(x) eine robuste Sch‰tzung der Stanardabweichung von x ist.
# 
# % default values for optional Parameters
# if nargin < 2 % ParetoRadiuss not given
#    ParetoRadius = ParetoRad(data);
# end;

	if(is.nan(paretoRadius)){
		paretoRadius <- paretoRad(data)
	}



	distanceMatrix <- as.matrix(dist(data),diag=T,upper=T)
	vectorOfDistance <- triuvec(as.matrix(dist(data)),k=1)
	
	inSphere <- (sqrt(distanceMatrix)<paretoRadius)
	nInSphere <- colSums(inSphere)
	denseThreshold <- 0.2013 * prctile((nInSphere/100),0.95)*100
	firstCenterInd <- which(nInSphere > denseThreshold)
	nFirstCenters <- length(firstCenterInd)




	PDEAtFirstCenters <- nInSphere[firstCenterInd]
	firstCenterDistance <- distanceMatrix[firstCenterInd,firstCenterInd]
	
	nearestDistance <- rep(0,nFirstCenters)
	nearestDensity <- rep(0,nFirstCenters)
	

	
	for(s in 1:nFirstCenters){
		noZeroInd <- which(firstCenterDistance[,s]>0)
		nearestDistance[s] <- min(firstCenterDistance[noZeroInd,s])
		minInd <- which.min(firstCenterDistance[noZeroInd,s])
		nearestDensity[s] <- PDEAtFirstCenters[noZeroInd[minInd]]
	}




	
	densityDifference <- nearestDensity-PDEAtFirstCenters       
	localDensityGradient <- densityDifference/nearestDistance
	
	denseFluctuation <- sd(localDensityGradient)*0.5
	

	
	newCenterInd <- which((abs(localDensityGradient) >denseFluctuation) | (PDEAtFirstCenters>2*denseThreshold))

	centersInd <- firstCenterInd[newCenterInd]



 return (list(centersInd=centersInd,nInSphere=nInSphere)) 

 }