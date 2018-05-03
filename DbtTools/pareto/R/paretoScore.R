`paretoScore` <- function(x,y,fast=FALSE){

# function [p,b] = ParetoScore(x,y,fast=0)
# % p = ParetoScore(x,y)
# %
# % Calulates classification error made when segmenting the range 
# % into intervals at the Bayesian decision boundaries, i.e.
# % all crossings of two probability density functions and 
# % using the denser class in each interval.
# % Both densities are estimated with the Pareto Density Estimation
# % unless a faster, suboptimal method is requested.
# %
# % INPUT
# % x,y       vector with values for two classes
# %
# % OPTIONAL
# % fast      fast version without pareto radius
# %
# % OUTPUT
# % p         Pareto score
# % b         Bayes error


	if(!fast){
		paretoRadius <- paretoRadius1D(rbind(x,y))
		pdeXVal <- paretoDensity1D(x,paretoRadius)
		pdeX <- pdeXVal$paretoDensity
		
		pdeYVal <- paretoDensity1D(y,paretoRadius)
		pdeY <- t(pdeYVal$paretoDensity)
		kernels <- pdeYVal$kernels
	}
	else {
		pdeXVal <- paretoDensity1D(x)
		pdeX <- pdeXVal$paretoDensity
		pdeYVal <- paretoDensity1D(y)
		pdeY <- pdeYVal$paretoDensity
		kernels <- pdeYVal$kernels
	}
# if (nargin < 3) || (fast == 0)
#     % slow version using pareto radius
#     ParetoRadius = ParetoRadius1D([x;y]);
#     [Kernels,pdeX] = ParetoDensity1D(x,ParetoRadius);
#     [Kernels,pdeY] = ParetoDensity1D(y,ParetoRadius,Kernels);
#     pdeY = pdeY';
# else
#     % fast version using stats toolbox
#     [pdeX,Kernels] = ksdensity(x);
#     [pdeY,Kernels] = ksdensity(y,Kernels);
# end
# 
# % bayesian classification error

# trapz funktion (caTools)
	
	pde <- pmin(pdeX,pdeY)
	idx = 2:length(kernels)
    b <- (as.double((kernels[idx] - kernels[idx - 1]) %*% (pde[idx] + pde[idx-1]))/2)


# b = trapz(Kernels,min(pdeX,pdeY));
  
  
# 
# % pareto score
# p = 1-b;
	p <- 1-b

 return(list(p=p,b=b)) 

 }

