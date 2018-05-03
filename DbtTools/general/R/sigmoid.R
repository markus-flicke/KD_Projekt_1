sigmoid <- function(x,m=0,t=1,plotIt=FALSE){
#  pointsGiven <- 0
  x <- as.vector(x)
# //TODO
#  if(length(m)==2 && length(t)==2){ # Calculate values for m and t based on the input vectors
#    pointsGiven <- 1
#    x0 <- m[1]
#    y0 <- m[2]
#    x1 <- t[1]
#    y1 <- t[2]
#    l0 <- log(1/y0-1)
#    l1 <- log(1/y1-1)
#    t <- (l0-l1)/(x1-x0)
#    m <- l0/t+x0
#    t <- t/4
#  }
#  if(length(m)==2 || length(t)==2){
#    print("The arguments t and m must have the same length")
#  }
  result <- 1/(1+exp(-4*t*(x-m)))
  if(plotIt == TRUE){
    # Prepare plot.
    b <- 0.5-t*m
    xg0 <- -b/t
    xg1 <- (1-b)/t
    plot(x,result,pch=20,col="blue",main="Sigmoid",ylab="Y-Axis",xlab="X-Axis")
    lines(c(xg0,xg1),c(0,1),col="green")
#    if(pointsGiven==1){
#      polyplot(c(x0,x1),c(y0,y1))
#    }
    return(result)
  }
  else{
    return(result)
  }
}