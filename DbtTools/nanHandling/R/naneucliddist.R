naneucliddist <- function(Data){
# [VectorOfDistances, DistanceMatrix] = naneucliddist(Data); 
# Euclidean distances between vectors containing NaN output in "pdist" and in matrix format
# let x,y be two row vectors of dimension n then: naneucliddist([x;y]) = sqrt(nanmean((x-y)^2*n));
#
# INPUT
# Data[1:NrOfCases,1:NrOfVariables]        the Data, cases in rows, variables in colums
# OUTPUT
# VectorOfDistances                        distance vector in same format as the output from the pdist function
# DistanceMatrix[1:NrOfCases,1:NrOfCases]  symmetric distance matrix with zeros along the diagonal
# in \dbt\NanHandling
#
NrOfVariables=nrow(Data)
NrOfCases=ncol(Data)
DistanceMatrix= matrix(0,nrow=NrOfCases,ncol=NrOfCases);# INIT

for (i in c(1:NrOfCases)){
		for (j in c((i+1):NrOfCases)){ # fill upper triangle
			if (j<=NrOfCases){
				Difference = Data[i,]-Data[j,];
				SquareDiff = Difference^2; 
				DistanceMatrix[i,j]   = sqrt(mean(SquareDiff, na.rm=TRUE)*NrOfVariables);
			}
		}
}
DistanceMatrix = DistanceMatrix + t(DistanceMatrix); # fill lower triangle

DistanceMatrix=as.matrix(DistanceMatrix);
VectorOfDistances =  as.vector(DistanceMatrix);   # same format as the output from the pdist function

 
 return (list(vectorOfDistances=VectorOfDistances,distanceMatrix=DistanceMatrix)) 

 }

