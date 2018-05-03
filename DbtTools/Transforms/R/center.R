`center` <- function(x){
	c <- t(t(x)-colMeans(x, na.rm=TRUE))
 	return (c) 
 }

