prctile<-function(x,p){
#   matlab:
#   Y = prctile(X,p) returns percentiles of the values in X. 
#   p is a scalar or a vector of percent values. When X is a 
#   vector, Y is the same size as p and Y(i) contains the p(i)th 
#   percentile. When X is a matrix, the ith row of Y contains the 
#   p(i)th percentiles of each column of X. For N-dimensional arrays,
#   prctile operates along the first nonsingleton dimension of X.  
  if(length(p)==1){  
            if(p>1){p=p/100}
            
  }

  if(is.matrix(x) && ncol(x)>1){
    cols<-ncol(x)
    quants<-matrix(0,nrow=length(p),ncol=cols)
    for(i in 1:cols){
      quants[,i]<-quantile(x[,i],probs=p,type=5,na.rm=TRUE)
    }
  }else{
    quants<-quantile(x,p,type=5,na.rm=TRUE)
  }
  return(quants)
}