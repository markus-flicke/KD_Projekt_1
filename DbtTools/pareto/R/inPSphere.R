`inPSphere` <- function(data,paretoRadius=NULL){

# function [AnzInPspheres] = InPSphere(Data,ParetoRadius,OLD1,OLD2);
# % [IsInSphere,InSphereInd,AnzInPspheres] = InPSphere(Kernels,ParetoRadius,Data);
# %  determine the data points inside a ParetoSphere with ParetoRadius
# %
# % INPUT
# % Data(d,n)                the data points in rows of dimension n 
# % OPTIONAL
# % ParetoRadius             radius of P-spherses; calculated if not given
# %                          if not given Data = Kernels
# %
# % OPTIONAL
# % OLD1,OLD2                ignored change interface !
# % 
# % OUTPUT
# % AnzInPspheres(d,1)       number of Data points inside a P-sphere with ParetoRadius 
# %
# 
# % / Pareto
# %  Author ALU 2008

	if(is.null(paretoRadius)){
		paretoRadius=paretoRad(data)	
	}

	data <- as.matrix(data)


	nData <- nrow(data)
	nVar <- ncol(data)
	
	if(nVar==2)
		nInPsphere <- inPSphere2D(data,paretoRadius)
	else{
		distVect <-c(dist(data,'euclid'))
		distVect <- distVect <= paretoRadius
		inSphere <- SquareForm(distVect)
		nInPsphere <- colSums(t(inSphere))	
	}
		


 return (nInPsphere) 

 }

