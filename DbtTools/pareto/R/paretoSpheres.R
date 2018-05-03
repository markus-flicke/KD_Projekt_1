`paretoSpheres` <- function(data, defined=NaN){

# function [NN, nc, pr] = ParetoSpheres(data, defined);
# % Calculate Pareto Spheres around each row in X
# %
# % INPUT
# % data    Data matrix with vectors in rows
# % defined vector of length d with 1 for columns to be used
# %         every column where defined is not 1 is discarded
# %         if defined is missing all components are used
# %
# % OUTPUT
# % NN      Matrix with indicators whether row i is in row j's Pareto Sphere
# % nc      Number of points in Pareto Sphere of row i
# % pr      Pareto Radius
# 


	data <- as.matrix(data)
	n <- nrow(data)
	d <- ncol(data)
	
	if(is.nan(defined))
		defined <- rep(TRUE,d)
	



	dd <- as.matrix(dist(data[,defined]),TRUE,TRUE)
	dp <- percentiles(triuvec(dd))
	pr <- dp[18]
	
	nn <- dd<pr
	nc <- rowSums(nn)


 return (list(nn=nn, nc=nc, pr=pr) ) 

 }

