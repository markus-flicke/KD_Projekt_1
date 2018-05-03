`ztransNaN` <- function(x){

	m <- colMeans(x,na.rm=TRUE)
	s <- sd(x,na.rm=TRUE)
	
	nRow <- dim(x)[1]
	nCol <- dim(x)[2]

	m <- matrix(m,nrow=nRow,ncol=nCol,byrow=TRUE)
	s <- matrix(s,nrow=nRow,ncol=nCol,byrow=TRUE)
	
	z <- (x-m)/s
	
	return(z)

 }

