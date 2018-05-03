`toRange` <-
function(data, lower, upper){
	
	data <- as.matrix(data)

	if(lower==upper){
		error('interval width can not be 0!')
	}

	if (lower > upper){
		temp <- upper;
   		upper <- lower;
   		lower <- upper;
	}
 
	range <- upper - lower
 
	n <- dim(data)[1]
	d <- dim(data)[2]

 	if ((n==1) & (d > 1)){ # row vector to colum vector
	   	data <- t(data)  
	   	wasRowVector <- 1
	   	}
 	else{
	   	wasRowVector <- 0
	}
 
 
 
 	nRow <- dim(data)[1]
 	nCol <- dim(data)[2]
 	
 	# Min = ones(Rows,1)*nanmin(data);
 	min <-apply(data,2,min,na.rm=TRUE)
 	min <- matrix(min,nRow,nCol,byrow=TRUE)

	

	# Max = ones(Rows,1)*nanmax(data);
	max <- apply(data,2,max,na.rm=TRUE)
	max <- matrix(max,nRow,nCol,byrow=TRUE)


	# Range = Max-Min;
	range <- max-min


	# Range(Range==0) =1; % falls Min==Max lass Daten in Ruhe

	range[range==0]<-1
	

	

	
	# ScaledData = (data-Min)./Range;          % scale to [0,1]

	scaleData <- (data-min)/range


		
# ScaledData = lower+ ScaledData *(upper-lower);  % scale to [lower, upper]

	scaleData <- lower + scaleData * (upper-lower)


	if(wasRowVector==1){
		scaleData = t(scaleData)
	}
	
	return(scaleData)
 }

