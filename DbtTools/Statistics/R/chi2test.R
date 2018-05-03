chi2test <- function(Data,bins=OptNrOfBins(Data), MC = TRUE){
# chi2test(Data,bins=OptNrOfBins(Data), MC = TRUE)
# function tests chi square test for given data against normal distribution,
# mean and standart deviation are estimated from given data
# example: chi2test(rnorm(100, 1, 1)) # tests if 100 data points of normal distribution (mean = 1, sd =1) are normal distributed

# INPUT
# Data			vector[1:n] with data to test
# OPTIONAL
# bins			integer number of bins, default optimal number of bins
# MC			boolean, if TRUE (default) monte carlo simulation for p-values are done, else not

# OUTPUT
# p-Value
# statistic		the value the chi-squared test statistic
# parameter		the degrees of freedom of the approximate chi-squared distribution of the test statistic, 
#				NA if the p-value is computed by Monte Carlo simulation
# observed		the observed counts
# expected		the expected counts under the null hypothesis

print('chi2test() is in development')

# observed data sorted in optimal number of bins
optBreaks <- seq(min(Data), max(Data), (max(Data)-min(Data))/bins)
beobacht <- hist(Data, breaks=optBreaks, plot=FALSE)

# expected data (same count as observed data) in same bins like observed data
m <- mean(Data)
s <- sd(Data)
num <- length(Data)
modell <- rnorm(n = num, mean = m, sd = s) # modell of normal distributed Data
modell[which(modell < 0)] <- 0 # chi2test do not like negativ numbers
erwart <- hist(modell, breaks=optBreaks, plot = FALSE)

if(!MC){ # no monte carlo simulation
	erg <- chisq.test(beobacht$counts, erwart$counts) 
}
if(MC){ # with monte carlo simulation
	erg <- chisq.test(beobacht$counts, erwart$counts, simulate.p.value = TRUE)
}

attach(erg)

return(list('p-Value' = p.value, statistic = statistic, parameter = parameter, observed = observed, expected = expected))
 

 }

