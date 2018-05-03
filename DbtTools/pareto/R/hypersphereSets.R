`hypersphereSets` <- function(data,hypersphereRadius){

# function [NrPointsInSphere,PSets] = HypersphereSets(Data,HypersphereRadius);
# % [NrPointsInSphere,PSets] = HypersphereSets(Data,HypersphereRadius);
# % calculate the number of points inside a hypersphere araund each data points
# % INPUT
# % Data(d,n)                Data matrix with vectors in rows
# % HypersphereRadius        Radius of Hyperspheres
# %
# % OUTPUT
# % NrPointsInSphere(d)      Number of data points in sphere around each point
# % PSets(d,n)               PSets(i,j) ==1 <=> Euclidian distance(Data(i,:),Data(j,:)) < HypersphereRadius  


	
	d <- SquareForm(c(dist(data,'euclid')))


	pSets <- d < hypersphereRadius
	nrPointsInSphere <- colSums(pSets)

 return (list(nrPointsInSphere=nrPointsInSphere, pSets=pSets)) 

 }

