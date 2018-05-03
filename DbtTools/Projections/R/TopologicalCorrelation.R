ToplogicalCorrelation <- function(Data,ProjectedPoints,type='norm',method,Kn=0){ 
# TC= ToplogicalCorrelation(Data,ProjectedPoints)
# TC= ToplogicalCorrelation(Data,method='CCA')  
# INPUT
# Data:               a matrix of the given n-dim. points: the rows represent the points and the columns represent the coordinates in the n-dim. space. 
# ProjectedPoints     matrix of Projected Points, if missing, method should be set!
# OPTIONAL
# method:             Determines whether the selected projections method for a given set of n-Dim. points is a good choice.
# 	                  Therefor, a result of 1 means the seleceted projections method is good, and a result value of 0 means that the Visualization of the given Data in the two dim. space doesnt fit for the problem.
# parameters
#	@KNN: k nearest neighbours in the graph. only needed in isomap and LocallyLinearEmbedding. 
	# 	@method: Prejection method that can be choosen out of CCA, ICA, isomap, LocallyLinearEmbedding, MDS, PCA, SammonsMapping, tSNE.
	# 	@type: how the paths in the adjacencematrix should be weighted, norm representes path lenthgs of 1 and eucldidean represents the distance in the euclidean metric.
	# @examples
	#	@ICA: ToplogicalCorrelation(Data,method='ICA',type='norm')
	#	@Isomap: ToplogicalCorrelation(Data,Kn=30,method='isomap',type='norm')
#
# Output
# TC      value
# author: Hermann, Tafo, Laukert Schlichting 07/2015
# 1.Editor: MT 07/2015: Interface verallgemeinert
#  requireRpackage('geometry') #delaunayn
if(!missing(method)){
	if(method == 'CCA'){
		pointsin2D <- do.call(CCA,list(Data,length(Data[,2])))$ProjectedPoints;
	}	 
	else if(method == 'ICA'){
		pointsin2D <- do.call(ICA,list(Data))$ProjectedPoints;
	}
	else if(method == 'isomap' || method == 'Isomap'){
		pointsin2D <- do.call(Isomap,list(Data,KNN=Kn))$ProjectedPoints;
	}
	else if(method == 'LLE'){
		pointsin2D <- do.call(LocallyLinearEmbedding,list(Data,KNN=Kn))$ProjectedPoints;
	}
	else if(method == 'MDS'){
		pointsin2D <- do.call(MDS,list(Data))$ProjectedPoints;
	}
	else if(method == 'PCA'){
		pointsin2D <- do.call(PCA,list(Data))$ProjectedPoints;
	}
	else if(method == 'SM'){
		pointsin2D <- do.call(SammonsMapping,list(Data))$ProjectedPoints;
	}
	else if(method == 'tSNE'){
		pointsin2D <- do.call(tSNE,list(Data))$ProjectedPoints;
	}
	else stop("The projection you've selected is not permitted");
}else{ #missing(method)
  pointsin2D=ProjectedPoints
}	#end !missing(method)
	stopifnot(!is.null(pointsin2D));	
	
	requireNamespace("GraphAlgorithms")
	if(type == 'norm'){
    
		matrixnD          <- matrix(0,length(Data[,1]),length(Data[,1]));
		delaumatrixnD     <- delaunayn(Data);
		matrix2D          <- matrix(0,length(pointsin2D[,1]),length(pointsin2D[,1]));	
		delaumatrix2D     <- GraphAlgorithms::DelaunayGraphMatrix(pointsin2D[,1],pointsin2D[,2])$TRI;

		for(i in 1:length(delaumatrixnD [,1])){
			for(j in 1:length(delaumatrixnD [1,])){
				if(j!=length(delaumatrixnD [1,])){				
					matrixnD[delaumatrixnD[i,j],delaumatrixnD[i,j+1]] = 1;
					matrixnD[delaumatrixnD[i,j+1],delaumatrixnD[i,j]] = 1;
				}			
				else{
					matrixnD[delaumatrixnD[i,j],delaumatrixnD[i,1]] = 1;
					matrixnD[delaumatrixnD[i,1],delaumatrixnD[i,j]] = 1;			
				}			
			}
		}

		for(i in 1:length(delaumatrix2D[,1])){
			for(j in 1:length(delaumatrix2D[1,])){
				if(j!=length(delaumatrix2D[1,])){
					matrix2D[delaumatrix2D[i,j],delaumatrix2D[i,j+1]] = 1;
					matrix2D[delaumatrix2D[i,j+1],delaumatrix2D[i,j]] = 1;
				}			
				else{
					matrix2D[delaumatrix2D[i,j],delaumatrix2D[i,1]] = 1;
					matrix2D[delaumatrix2D[i,1],delaumatrix2D[i,j]] = 1;			
				}			
			}
		}

		adjazenceMatrix <- list(matrixnD =matrixnD,matrix2D=matrix2D);
		domain          <- GraphAlgorithms::ShortestGraphPaths(adjazenceMatrix$matrixnD)$Cost;
		codomain        <- GraphAlgorithms::ShortestGraphPaths(adjazenceMatrix$matrix2D)$Cost;		
		n               <- length(domain[2,]);
		m               <- length(codomain[2,]);
		d_g.raw         <- 0;
		d_v.raw         <- 0;	
		enumerator      <- 0;
		denominator_g   <- 0;
		denominator_v   <- 0;

		stopifnot(n == m);

		for(i in 2:n){
			for(j in 1:(i-1)){
				d_g.raw <- d_g.raw + domain[i,j];	
				d_v.raw <- d_v.raw + codomain[i,j];		
			}
		}	

		d_g <- d_g.raw/((n*(n-1))/2);
		d_v <- d_v.raw/((n*(n-1))/2); 	

		for(i in 2:n){
			for(j in 1:(i-1)){
				enumerator    <- enumerator    + ((domain[i,j]  - d_g) * (codomain[i,j] - d_v));
				denominator_g <- denominator_g + (domain[i,j]   - d_g)^2;
				denominator_v <- denominator_v + (codomain[i,j] - d_v)^2;
			}
		}
		T_c <- enumerator/sqrt(denominator_g*denominator_v);
	}
	
	else if(type == 'euclidean'){

		distancematrixnD  <- matrix(0,length(Data[,1]),length(Data[,1]));
		delaumatrixnD     <- delaunayn(Data);
		distancematrix2D  <- matrix(0,length(pointsin2D[,1]),length(pointsin2D[,1]));	
		delaumatrix2D     <- GraphAlgorithms::DelaunayGraphMatrix(pointsin2D[,1],pointsin2D[,2])$TRI;

		for(i in 1:length(delaumatrixnD [,1])){
			for(j in 1:length(delaumatrixnD [1,])){
				if(j!=length(delaumatrixnD [1,])){
					distancematrixnD[delaumatrixnD[i,j],delaumatrixnD[i,j+1]] = sqrt(sum((Data[delaumatrixnD[i,j],]   - Data[delaumatrixnD[i,j+1],])^2));
					distancematrixnD[delaumatrixnD[i,j+1],delaumatrixnD[i,j]] = sqrt(sum((Data[delaumatrixnD[i,j+1],] - Data[delaumatrixnD[i,j],])^2));
				}			
				else{
					distancematrixnD[delaumatrixnD[i,j],delaumatrixnD[i,1]]   = sqrt(sum((Data[delaumatrixnD[i,j],]   - Data[delaumatrixnD[i,1],])^2));
					distancematrixnD[delaumatrixnD[i,1],delaumatrixnD[i,j]]   = sqrt(sum((Data[delaumatrixnD[i,1],]   - Data[delaumatrixnD[i,j],])^2));				
				}			
			}
		}

		for(i in 1:length(delaumatrix2D [,1])){
			for(j in 1:length(delaumatrix2D [1,])){
				if(j!=length(delaumatrix2D [1,])){
					distancematrix2D[delaumatrix2D[i,j],delaumatrix2D[i,j+1]] = sqrt(sum((pointsin2D[delaumatrix2D[i,j],]   - pointsin2D[delaumatrix2D[i,j+1],])^2));
					distancematrix2D[delaumatrix2D[i,j+1],delaumatrix2D[i,j]] = sqrt(sum((pointsin2D[delaumatrix2D[i,j+1],] - pointsin2D[delaumatrix2D[i,j],])^2));
				}			
				else{
					distancematrix2D[delaumatrix2D[i,j],delaumatrix2D[i,1]]   = sqrt(sum((pointsin2D[delaumatrix2D[i,j],]   - pointsin2D[delaumatrix2D[i,1],])^2));
					distancematrix2D[delaumatrix2D[i,1],delaumatrix2D[i,j]]   = sqrt(sum((pointsin2D[delaumatrix2D[i,1],]   - pointsin2D[delaumatrix2D[i,j],])^2));				
				}			
			}
		}
		adjazenceMatrix <- list(distancenD =distancematrixnD,distance2D=distancematrix2D );
		domain          <- GraphAlgorithms::ShortestGraphPaths(adjazenceMatrix$distancenD,adjazenceMatrix$distancenD)$Cost;
		codomain        <- GraphAlgorithms::ShortestGraphPaths(adjazenceMatrix$distance2D,adjazenceMatrix$distance2D)$Cost;
		n               <- length(domain[2,]);
		m               <- length(codomain[2,]);
		d_g.raw         <- 0;
		d_v.raw         <- 0;	
		enumerator      <- 0;
		denominator_g   <- 0;
		denominator_v   <- 0;

		stopifnot(n == m);

		for(i in 2:n){
			for(j in 1:(i-1)){
				d_g.raw <- d_g.raw + domain[i,j];	
				d_v.raw <- d_v.raw + codomain[i,j];		
			}
		}	

		d_g <- d_g.raw/((n*(n-1))/2);
		d_v <- d_v.raw/((n*(n-1))/2);	

		for(i in 2:n){
			for(j in 1:(i-1)){
				enumerator    <- enumerator    + ((domain[i,j]  - d_g) * (codomain[i,j] - d_v));
				denominator_g <- denominator_g + (domain[i,j]   - d_g)^2;
				denominator_v <- denominator_v + (codomain[i,j] - d_v)^2;
			}
		}
	T_c <- enumerator/sqrt(denominator_g*denominator_v);
	}
	else stop('Permitted types: -norm- , -euclidean-');	

	return(T_c);	
}

