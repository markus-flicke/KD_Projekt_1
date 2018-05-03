`PDEtrimstd` <- function(data,trimPercentile){

# function TrimmedSdev = PDEtrimstd(Data,TrimPercentile);
# % TrimmedSdev = PDEtrimstd(Data,TrimPercentile);
# % std trimmed to the most dense ragions in Data
# % die top 100-TrimPercentile der PDEstimationFuerGauss(Data)
# % werden fuer die Berechnung von std vewendet
# % INPUT
# % OUTPUT
# %
# % ...dbt\Pareto
# % ALU 2005

	data <- as.matrix(data) 
	nCases <- nrow(data)
	nVar <- ncol(data)
	


	if(max(trimPercentile)>1)	# trimPercentile  outside [0,1]
		trimPercentile <- trimPercentile/100
		
		
	if(nVar==1){
		pdeEst <- PDEstimationForGauss(data,0,data)
		pdfQuantile <- prctile(pdeEst$paretoDensity,trimPercentile)
		densInd <- pdeEst$paretoDensity > pdfQuantile
		
		if(length(sum(densInd))>0)
			trimmedSdev <- sd(pdeEst$kernels[densInd])
		else
			trimmedSdev <- 0
	}
	else {
		
		trimmedSdev <- rep(0,nVar)
		for(i in 1:nVar){
			pdeEst <- PDEstimationForGauss(data[,i],0,data[,i])
			pdfQuantile <- prctile(pdeEst$paretoDensity,trimPercentile)
			densInd <- pdeEst$paretoDensity > pdfQuantile
			
			if(length(sum(densInd))>0)
				trimmedSdev[i] <- sd(pdeEst$kernels[densInd])
			else
				trimmedSdev[i] <- 0

		}
	}


 return (trimmedSdev) 

 }

