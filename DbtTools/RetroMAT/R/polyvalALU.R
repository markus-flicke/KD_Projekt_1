polyvalALU <- function(p, x){
# function to evaluate for input vektor with input polynom output vektor of same length

# INPUT
# p		vector of polynom coefficients in descending power
# x 	vector of data which should be evaluated

# OUTPUT
# y		vector of data which is evaluated by polynom p and input data x

n <- length(p)
polynom <- c()

if(n > 1){
	for(i in c(1:n-1)){
		pol <- x*p[i]
		polynom <- cbind(polynom,pol)
	}
}

pol <- x*p[n]
polynom <- cbind(polynom,pol)
y <- rowSums(polynom)

return(y)
}