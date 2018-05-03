normpdf <- function(X,Mean,Sdev){
# computes the normal pdf at each of the values in X 
# R funktion fuer PDF = normpdf(X,Mean,Sdev)
return(dnorm(X,Mean,Sdev))
} # end function normpdf