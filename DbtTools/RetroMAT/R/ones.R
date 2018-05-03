ones <-function (n,m=n) {
# ones(n)   returns an n-by-n matrix of 1s. 
# ones(n,1) returns a vector of 1s 
# ones(n,m) returns an n-by-m matrix of ones.

if (m==1) { # vector wird zurueckgegeben
   return(c(1:n)*0+1) ;
}else{      # return n-by-m matrix of ones.
  return(matrix(1,n,m));
} # end if (m==1) 

} # end function  ones

