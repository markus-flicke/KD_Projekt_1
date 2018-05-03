PDEgmm=function(kernels,paretoDensity,means,stds=NaN,alphas=NaN,bins=NULL,bw=FALSE){
#require('matlab')
# function h = PDEgmm(Kernels,ParetoDensity,Means,Stds,Alphas,Bins,bw);
# % h = PDEgmm(data,mean,std,alpham,bins);
# %
# % plot pde with gaussian mixtures and bins
# %
# % INPUT
# % Kernels              kernels from PDE
# % ParetoDensity        density values at kernels
# % Means                means of mixtures
# %
# % OPTIONAL
# % Stds                 stds of mixtures
# % Alphas               weights of mixtures
# % Bins                 bin boundaries
# % bw                   black and white plots
# %
# % OUTPUT
# % h                     handle to plot
# %
# % in $Source: /data/CVS/ag/dbt/Pareto/PDEgmm.m,v $
# %
# % $Author: ultsch $
# % $Revision: 1.4 $
# %
# % HISTORY
# % $Log: PDEgmm.m,v $
# % Revision 1.4  2005/07/19 05:57:22  ultsch
# % no message
# %
# % Revision 1.2  2005/05/12 12:29:28  fabian
# % *** empty log message ***
# %
# % Revision 1.1  2005/04/13 10:03:56  fabian
# % *** empty log message ***
# %
# 
# % plot mixture of gaussians

 
   

lenM <- length(means)

	if(is.nan(alphas))
		alphas <- repmat(1/lenM,1,lenM)
		
	if(is.nan(stds))
		stds <- repmat(1,1,lenM)		
		
		
		
	mixtures <- matrix(0,length(kernels),length(alphas)+1)

	m <- ncol(mixtures)-1
	



	for(i in 1:m)
		mixtures[,i] <- alphas[i]*dnorm(kernels,means[i],stds[i])


	mixtures[,m+1] <- rowSums(mixtures[,1:m])


	cr <- rainbow(m)
	
	plot(kernels,paretoDensity,type='l',lwd=2,ylab='',xlab='')
	
	if(bw) {
		lines(kernels,mixtures[,m+1],lwd=2,lty=2)
	}

	for(i in 1:m){
		if(bw)
			lines(kernels,mixtures[,i],lwd=2,lty=2)
		else 
			lines(kernels,mixtures[,i],col=cr[i],lwd=2,lty=2)
	}
	
	
	if(!is.null(bins)){
		if(bw)
			abline(v=bins)
		else
			abline(v=bins,col='red')		
	}



 }

