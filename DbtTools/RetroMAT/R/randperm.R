randperm <- function(n,k = n){
	p <-sample(c(1:n),k)
	return(p)
}