`paretoDensity1D` <- function(data,paretoRadius=0){
  # MT: Dient zur schneller Abschätzung der Dichte, normalerweise immmer ParetoDensityEstimation verwenden
# function [Kernels,ParetoDensity,ParetoRadius] = ParetoDensity1D(Data,ParetoRadius,Kernels);
# % [Kernels,ParetoDensity,ParetoRadius] = ParetoDensity1D(Data,ParetoRadius,Kernels);
# %
# % in /dbt/Pareto/
# %
# % DESCRIPTION
# % Calculate the ParetoDensity for a one dimensional distribution.
# %
# % INPUT
# % Data                    data vector
# %
# % OPTIONAL
# % ParetoRadius            Pareto Radius, default see ParetoRadius1D
# % Kernels                 data values at which ParetoDensity is measured
# %
# % OUTPUT
# % Kernels                 data values at which ParetoDensity is measured
# % ParetoDensity           the estimated density
# % ParetoRadius            Pareto Radius
# %
# % AUTTOR : ALU,FAB
# 
# % remove NaN bevore flight

	data <- data[!is.nan(data)]
# Data = Data(find(not(isnan(Data))));

	if(!is.vector(data)){
		print('paretoDensity1D: only 1D data accepted')
		return()
	}
	
	if(paretoRadius==0){
		paretoRadius <- paretoRadius1D(data)
	}
# % determine pareto radius
# if (nargin < 2) || (length(ParetoRadius) == 0) || (ParetoRadius == 0)
#   ParetoRadius = ParetoRadius1D(Data);
# end;
# 
# % kernel density estimation
# if (nargin < 3)
#     [ParetoDensity,Kernels] = ksdensity(Data,'kernel','box','width',ParetoRadius);
#     ParetoDensity = ParetoDensity';
# else
#     [ParetoDensity,Kernels] = ksdensity(Data,Kernels,'kernel','box','width',ParetoRadius);    
# end

	ksdensityVal <- density(data,bw=paretoRadius,n=100,width=paretoRadius,kernel="rectangular")

 return (list(kernels=ksdensityVal$x,paretoDensity=ksdensityVal$y,paretoRadius=paretoRadius))

 }

