`paretoClasses` <- function(data,verbose=FALSE,paretoRadius=0){

# function [ABCDcls,ParetoRadius,AnzInPspheres,DensitityLimits] = ParetoClasses(Data,Verbose,ParetoRadius);
# % [ABCcls,ParetoRadius] = ABCDensityClasses(Data);
# % calculate the ABCD classes for PDE & estmate ParetoRadius
# % INPUT
# % Data(1:n,1:d)             data array, cases in rows, variables in columns
# % OPTIONAL
# % Verbose                 ==1 give some hints about what i am doing, 0 by default
# % ParetoRadius            the Pareto Radius if given and nozero than this
# %                         is used otherwise it is calculated using ParetoRad(...)
# % OUTPUT
# % ABCDcls(1:n)            density classification of Data Acls = 1; Bcls = 2; Ccls = 3; Dcls = 4;  
# % ParetoRadius            the Pareto Radius
# % AnzInPspheres(1:n)      numnber of points in ParetoSphere around each data point
# % DensitityLimits        =  [AdensityLimit;BdensityLimit;CdensityLimit];
# %                           such that    Density_of(Acls_Data)>AdensityLimit    ...
# %                                     >= Density_of(Bcls_Data)>BdensityLimit    ...
# %                                     >= Density_of(Ccls_Data)>CdensityLimit >= Density_of(Dcls_Data)
# 
# % Author: A. Ultsch 2004
# 
#  CONSTANTS
	aPercent <- 20
	bPercent <- 57
	cPercent <- 80
	
	# identifiers for the ABCD classes in ABCDcls
	aCls <- 1
	bCls <- 2
	cCls <- 3
	dCls <- 4 



	if (paretoRadius==0){
		maximumNrSamples <- 1000
		expectedNumberOfClusters <- 6  #see ULTSCH03  0.33 as InnerToInter correction
		plotDistancePercentiles <- TRUE # No Plot
		verbose <- TRUE 
		print('Calculation of Pareto Radius with 0.33 as InnerToInter correction')
		print('Pareto Radius:')
		paretoRadius <- paretoRad(data,maximumNrSamples,expectedNumberOfClusters,plotDistancePercentiles,verbose)
	}
		
		
	# ParetoRadius fuer nur einen gesamtcluster


	#plot('Calculation of ParetoSpheres')
	nInPspheres <- inPSphere(data,paretoRadius)
	sum(nInPspheres)

	aDensityLimit <- prctile((nInPspheres/100),((100-aPercent)/100))*100
	bDensityLimit <- prctile((nInPspheres/100),((100-bPercent)/100))*100
	cDensityLimit <- prctile((nInPspheres/100),((100-cPercent)/100))*100
	densitityLimits <-  rbind(aDensityLimit,bDensityLimit,cDensityLimit)


	if (verbose){
		dev.new()
		PDEplot(nInPspheres)
		x <- axTicks(1)
		y <- axTicks(2)
		points(c(aDensityLimit,aDensityLimit),c(0,tail(y,n=1)*2),type='l',col='green')
		points(c(bDensityLimit,bDensityLimit),c(0,tail(y,n=1)*2),type='l',col='blue')
		points(c(cDensityLimit,cDensityLimit),c(0,tail(y,n=1)*2),type='l',col='red')
			
		
		yText <- tail(y,n=1)/2		
		aTextX <- aDensityLimit+0.5*(tail(x,n=1)-aDensityLimit)
		bTextX <- bDensityLimit+0.5*(aDensityLimit-bDensityLimit)
	 	cTextX <- cDensityLimit+0.5*(bDensityLimit-cDensityLimit)
		dTextX <- cDensityLimit*0.5
		text(aTextX,yText,'A')
		text(bTextX,yText,'B')
		text(cTextX,yText,'C')
		text(dTextX,yText,'D')
		title(main='PDE density classes')
		title(xlab='# points in ParetoSpheres')
				
	}

#  Classify
 	aInd <- nInPspheres>aDensityLimit
	bInd <- (nInPspheres<=aDensityLimit)&(nInPspheres>bDensityLimit)
	cInd <- (nInPspheres<=bDensityLimit)&(nInPspheres>cDensityLimit)

	abcdCls <- nInPspheres*0+dCls #init
	abcdCls[aInd] <- aCls
	abcdCls[bInd] <- bCls
	abcdCls[cInd] <- cCls

 invisible(list(abcdCls=abcdCls,paretoRadius=paretoRadius,nInPspheres=nInPspheres,densitityLimits=densitityLimits)) 

 }

