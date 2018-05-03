polyfit <- function(x,y,n, r = FALSE){

# finds the coefficients of a polynomial p(x) of degree n that fits the data, p(x(i)) to y(i)

# INPUT
# x		vektor mit x Werten
# y		vektor mit f(x) Werten
# n		Grad des Polynom
# r		if TRUE, use raw and not orthogonal polynomials, default: FALSE

# OUTPUT
# p		p is a row vector of length n+1 containing the polynomial coefficients in descending powers
#		


ptemp <- lm(y ~ poly(x, degree = n, raw = r))
p <- ptemp$coefficients
names(p) <- NULL
p <- rev(p)

return(p)
}