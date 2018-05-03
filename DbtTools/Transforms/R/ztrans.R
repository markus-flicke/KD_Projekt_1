`ztrans` <- function(x){
	
	if(any(is.nan(x))||any(is.infinite(x))) {
		ztransNaN(x)
	}
	
	else{
	
		m <- dim(x)[1]

		sdev <- sd(x, na.rm = TRUE)
		z <- x- t(kronecker(matrix(1,1,m),colMeans(x,na.rm=TRUE)))
	
		if(max(sdev,na.rm=TRUE) >0 ){
			z[,sdev!=0] <- z[,sdev!=0] / t(kronecker(matrix(1,1,dim(z)[1]),sdev[sdev!=0]))

		}

		return(z)
	}
}

