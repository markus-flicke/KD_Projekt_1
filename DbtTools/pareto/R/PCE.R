
`PCE` <- function(data,paretoRadius=NaN){

# function [Kernels,IntegralParetoDensity,ParetoRadius] = PCE(Data,ParetoRadius);
# % [Kernels,IntegralParetoDensity,ParetoRadius] = PCE(Data,ParetoRadius);
# % numerically integration of the ParetoDensityEstimation using
# % trapezoid method%
# % INPUT
# % Data                    die eindimensional verteilten Daten
# % ParetoRadius            der Pareto Radius, wenn nicht angegeben, wird er berechnet
# %
# % OUTPUT
# % Kernels                 data values at which ParetoDensity is measured , use plot(Kernels,ParetoDensity) for display
# % ParetoDensity           die mit dem ParatoRadius ermittelte Dichte
# % ParetoRadius            der Pareto Radius
# %
# % Author: ALU 2004
# % 
# % in /dbt/Pareto/


	if(is.nan(paretoRadius))
		paretoRadius=ParetoRadius(data)	
		
	pdeVal <- ParetoDensityEstimation(data,paretoRadius)
	
	
	
	# trapz funktion ( aus caTools)
	area <- rep(0,length(pdeVal$kernels))
	
	
	for (i in 2:length(pdeVal$kernels)){
		idx = 2:i
    	area[i] <- (as.double((pdeVal$kernels[idx] - pdeVal$kernels[idx - 1]) %*% (pdeVal$paretoDensity[idx] + pdeVal$paretoDensity[idx - 1]))/2)
	}
	
 return (list(kernels=pdeVal$kernels,integralParetoDensity=area,paretoRadius=pdeVal$paretoRadius)) 

 }

