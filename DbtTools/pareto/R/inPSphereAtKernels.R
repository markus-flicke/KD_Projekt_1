`inPSphereAtKernels` <- function(kernels,paretoRadius=NULL,data=NULL,kernels2DataDistance=NULL){

# function [IsInSphere,InSphereInd,AnzInPspheres,Kernels2DataDistance] = InPSphereAtKernels(Kernels,ParetoRadius,Data,Kernels2DataDistance);
# % [IsInSphere,InSphereInd,AnzInPspheres] = InPSphereAtKernels(Kernels,ParetoRadius,Data);
# % for given points (Kernels) determine the data points inside a ParetoSphere with ParetoRadius
# %
# % INPUT
# % Kernels(AnzKernels,n)    the centers at which spheres with PatetoRadius are considered
# % OPTIONAL
# % ParetoRadius             radius of P-spherses; calculated if not given
# % Data(d,n)                the data points in rows of dimension n 
# %                          if not given Data = Kernels
# % Kernels2DataDistance      Kernels2DataDistance(i,:) contains all distances of  Kernels(i,:) to Data
# %
# % OUTPUT
# % IsInSphere(d,1)                binary vector: IsInSphere(i) <=> Data(i) is in (at least) one P-sphere
# % InSphereInd                    index vector of those Data points in at least one P-sphere 
# % AnzInPspheres(AnzKernels,1)    number of Data points inside a P-sphere with ParetoRadius around Kernels 
# %
# % / Pareto
# %  Author ALU 2004
# 

	if(is.null(data))	
		data=kernels
	
	if(is.null(paretoRadius))
		paretoRadius <- paretoRad(data)
		
	nKernels <- nrow(kernels)
	n <- ncol(kernels)
	
	nData <- nrow(data)
	nVar <- ncol(data)

	if(n!=nVar){
		print('ERROR - InPSphere: dimension of Kernels not equal to dimension of Data')
		return()
	}


	if(!is.null(kernels2DataDistance))
		distancesGiven = TRUE
	else{
		if((nKernels*nData)<100000){
			kernels2DataDistance <- distA2B(kernels,data)
			distancesGiven = TRUE
		}
		else
			distancesGiven = FALSE
				
	}


	if(all.equal(kernels,data)){
		kernels2DataDistance <- SquareForm(c(dist(data,'euclid')))
		distancesGiven = TRUE
	}
		

	isInSphere <- rep(0,nData)
	nInPsphere <- rep(0,nKernels)
		
	for(i in 1:nKernels){
		kernel <- kernels[i,]
		if(distancesGiven)	
			distanzToKernel <- t(kernels2DataDistance[i,])
		else 
			distanzToKernel <- dist2all(kernel,data)
			
		inKernelSphereInd <- distanzToKernel<=paretoRadius
		nInPsphere[i] <- sum(inKernelSphereInd)
		isInSphere(inKernelSphereInd) = TRUE

		if((nKernels*nData)> 100000){
			histopt(nInPsphere[1:i],paste(i,' = ',round(i/nKernels*100),' % of kernels calculated'))
		#	xlabel(['points in pareto spheres of radius ' num2str(ParetoRadius)]); ylabel('nr of occurences');
		
		}
	}
	
	

	inSphereInd <- which(isInSphere)

 return (list(isInSphere= isInSphere,inSphereInd=inSphereInd,nInPspheres= nInPspheres,kernels2DataDistance=kernels2DataDistance) ) 

 }

