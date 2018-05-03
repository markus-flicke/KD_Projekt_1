`PDEModes` <- function(data){

# function [modes, heights, areas, kernels, pde] = PDEModes(data);
# % [modes, heights, areas, kernels, pde] = PDEModes(data);
# %
# % in \dbt\Pareto\
# %
# % DESCRIPTION
# % Find mode(s) of PDE. So far only one is searched.
# %
# % INPUT
# % Data               Vector of Data
# %
# % OUTPUT
# % modes              x value(s) of mode(s)
# % heights            y value(s) of mode(s)
# % areas              percentiles in mode bins
# % kernels            returned from PDEstimationFuerGauss
# % pde                returned from PDEstimationFuerGauss
# 
# % Version: FAB 4.7.04 (first version)
# 

	pdeVal <- PDEstimationForGauss(data)
	ind <- which.max(pdeVal$paretoDensity)
	modes <- pdeVal$kernels[ind]
	heights <- pdeVal$paretoDensity[ind]
	
	
	areas <- rep(0,length(modes)+1)
	areas[1] <- sum(data<modes[1])
	areas[length(modes)+1] <- sum(data>modes[length(modes)])
	
	areas <- areas/length(data)



 return (list(modes=modes, heights=heights, areas=areas, kernels=pdeVal$kernels, pde=pdeVal$paretoDensity)) 

 }

