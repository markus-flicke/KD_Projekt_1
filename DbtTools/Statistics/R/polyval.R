polyval <- function(v,x) {
   
   n <- length(v)
   y <- x*0+v[1];
   for (i in 2:n) {
     y <- v[i] +x*y
   }
   return(y)
 }