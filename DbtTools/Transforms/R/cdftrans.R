`cdftrans` <-function(x){


	m <- dim(x)[1]				#dim row
	n <- dim(x)[2]				#dim col
	c <- matrix(0,m,n)	#init 

	if(n==1){
		for(i in seq(m)){
			c[i,1] = sum(x <= x[i,1],na.rm=TRUE)
		}
		c=c/m
	}
	else {
			for(col in seq(n)){
			
					for(i in seq(m)){					
					c[i,col] <-sum((x[,col]<=x[i,col]),na.rm=TRUE)
				}
									
			}
				c <- c/m	
	}
 return (c) 

 }

