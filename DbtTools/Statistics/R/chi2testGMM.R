chi2testGMM <- function(Data, Means, Sdevs, Weights = rep(1/length(Means),length(Means)), IsLogDistribution = Means*0, Repeat = 1){
# example for normal distribution:

# function makes chi square test for given data and given gauss mixture modell
# function uses R statistics function chisq.test()

# INPUT
# Data[1:n]					vector of Data thats distribution was modelled
# Means[1:L]				vector with means of L gauss curves of the modell
# Sdevs[1:L]				vector with variances of L gauss curves of the modell
# OPTIONAL
# Weights[1:L]				vector with weights of L gauss curves of the modell, default all gauss are same weighted
# IsLogDistribution[1:L]	vector with 0 or 1: 0 means "not log distribution", 1 means "gauss is log distributed", (default no Gauss is log)
# Repeat					integer to give the number of repeats for choose random data from modell, default only one cycle of choose and test

# OUTPUT
# pValue					double which gives the p-value for this test
# 

modell <- randomLogMix(Means, Sdevs, Weights, IsLogDistribution, TotalPoints = length(Data))
erg <- chisq.test(Data,modell)


print('chi2testGMM() is in development')
res <- list(pValue = pValue)
return(res)
}