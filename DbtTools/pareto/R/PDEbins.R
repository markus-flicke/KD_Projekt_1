`PDEbins` <- function(kernels,paretoDensity,bins,numbers=FALSE){

# function h = PDEbins(Kernels,ParetoDensity,Bins,Numbers);
# % h = PDEbins(Kernels,ParetoDensity,Bins);
# %
# % plot pde with vertical lines for dicretization bins
# %
# % INPUT
# % Kernels               kernels from PDE
# % ParetoDensity         density values at kernels
# % Bins                  vector of bin boundaries
# %
# % OPTIONAL
# % Numbers               whether to show numbers
# %
# % OUTPUT
# % h                     handle to plot
# %
# % in $Source: /data/CVS/ag/dbt/Pareto/PDEbins.m,v $
# %
# % $Author: fabian $
# % $Revision: 1.5 $
# %
# % HISTORY
# % $Log: PDEbins.m,v $
# % Revision 1.5  2005/10/13 13:14:28  fabian
# % *** empty log message ***
# %
# % Revision 1.4  2005/07/19 05:57:22  ultsch
# % no message
# %
# % Revision 1.2  2004/10/02 19:27:54  fabian
# % what can you say
# %
# % Revision 1.1  2004/09/29 18:26:24  fabian
# % more goodies
# %

	plot(kernels,paretoDensity,col='blue',xlab='Data',ylab='PDE',type='l')
	y <- tail(axTicks(2),n=1)
	
	for (k in 1:length(bins)){
		points(c(bins[k],bins[k]),c(0,y*2),type='l',col='red')
	}
	
	if(numbers){
		x <- axTicks(1)
		y <- tail(axTicks(2),n=1)
	
		for(i in 1:length(bins)){
			text(x[i],y,i,adj=-0.5)
		}
	}

 }

