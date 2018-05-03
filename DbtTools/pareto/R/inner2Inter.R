`inner2Inter` <-function(expectedNumberOfClusters){

# function [RatioInner2InterClusterDistances] = Inner2Inter(ExpectedNumberOfClusters);
# % [RatioInner2InterClusterDistances] = Inner2Inter(ExpectedNumberOfClusters);
# % Calculation of the expected Ratio of Inner vs. inter Cluster distances   
# % see [Ultsch03:  Optimal density estimation in data containing clusters of unknown structure] for details
# % Author: A. Ultsch 2003
# % INPUT
# % ExpectedNumberOfClusters  the number of clusters that are expected in a data set
# %                           NOTE: if the ExpectedNumberOfClusters is not known we
# %                           recommend  to use ExpectedNumberOfClusters=6
# % OUTPUT
# % RatioInner2InterClusterDistances   modify the ParetoPercentile as follows: 
# % RatioInner2InterClusterDistances * ParetoPercentile  = Right Percentile
# %                           of distance uded to calculate a Pareto density
# %                           estimation
# 
# % check Limits
# if ExpectedNumberOfClusters < 2 ,   ExpectedNumberOfClusters=1; end;
# if ExpectedNumberOfClusters > 40 ,  ExpectedNumberOfClusters=40; end;
# I2I = zeros(40,1); %Init
#  number of clusters	mean of ratio intra/inter cluster distances
	
	I2I <- vector(mode = "numeric", length = 40)
	
	if(expectedNumberOfClusters<1)
		expectedNumberOfClusters <- 1
	
	if(expectedNumberOfClusters>40)
		expectedNumberOfClusters <- 40
	
	I2I[1] <- 1
	I2I[2] <- 0.673655
	I2I[3] <- 0.540071
	I2I[4] <- 0.448394
	I2I[5] <- 0.380795
	I2I[6] <- 0.3263
	I2I[7] <- 0.286768
	I2I[8] <- 0.25035
	I2I[9] <- 0.225546
	I2I[10] <- 0.202865
	I2I[11] <- 0.185515
	I2I[12] <- 0.170194
	I2I[13] <- 0.157708
	I2I[14] <- 0.145808
	I2I[15] <- 0.136427
	I2I[16] <- 0.127417
	I2I[17] <- 0.119922
	I2I[18] <- 0.113884
	I2I[19] <- 0.10755
	I2I[20] <- 0.102236
	I2I[21] <- 0.097573
	I2I[22] <- 0.09319
	I2I[23] <- 0.088828
	I2I[24] <- 0.085517
	I2I[25] <- 0.081595
	I2I[26] <- 0.078621
	I2I[27] <- 0.075995
	I2I[28] <- 0.072974
	I2I[29] <- 0.070437
	I2I[30] <- 0.101487
	I2I[31] <- 0.097475
	I2I[32] <- 0.09462
	I2I[33] <- 0.091768
	I2I[34] <- 0.089551
	I2I[35] <- 0.08696
	I2I[36] <- 0.084779
	I2I[37] <- 0.082454
	I2I[38] <- 0.080437
	I2I[39] <- 0.078333
	I2I[40] <- 0.076277

return(ratioInner2InterClusterDistances = I2I[expectedNumberOfClusters])

 

 }

