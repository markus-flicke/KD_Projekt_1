`differencesDistribution` <- function(feature, plotIt=FALSE){


	maximumNrSamples <- 4000 # Maximum number of data sets for which the difference calculation can be done


 	feature <- feature[is.finite(feature)]
 	nData	<- length(feature)

	if (maximumNrSamples >= nData){ # no sampling necessary
    	sampleF <- feature
    }
	else #  sample with uniform distribution MaximumNrSamples  
	{
		sampleInd <- floor(nData*c(runif(maximumNrSamples))+1)
		sampleF <- feature[sampleInd,]
 
	}


	upperTriangle <- c(dist(sampleF,'manhattan'))
	 
	p4060 <- prctile(upperTriangle,0.1)*4  
	
	p4060Factor <- 1.9736 
	innerDeviation = p4060*p4060Factor


	if(plotIt){
		maxNrDistances  <- 30000
		nDist = length(upperTriangle)
		
		print(nDist)
		
		if(nDist>maxNrDistances){ #sample with uniform distribution MaximumNrSamples 
			sampleInd <- floor(nDist*c(runif(maxNrDistances))+1)
			upperTriangle <- upperTriangle[sampleInd]
		}
		
		differences <- as.matrix(c(upperTriangle,-upperTriangle),ncol=1)
			
		pdeN <- pdeNorm(differences[,1],yl='bl= PDF of Differences, mg = sdev, gn= InnerDeviation',main='Differences Distribution')
				
		gauss <- dnorm(pdeN$kernels,0,innerDeviation)
		fitGauss <- gauss/prctile(gauss,0.99)*max(pdeN$paretoDensity)
		points(pdeN$kernels,fitGauss,col='green',type='l')
		grid()
	}


 return (list(innerDeviation=innerDeviation,differences=differences)) 
}