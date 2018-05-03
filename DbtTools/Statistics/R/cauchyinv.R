cauchyinv <- function(p, ml=0.0,md=1.0){

 mdInd=which(md<=0)
 if(length(mdInd)>0) md[mdInd]=NaN
 p[p<0 | p>1]=NaN
 x=ml + md*tan(pi*(p-0.5))
 if(length(p)==1) 
 pnew=repmat(p,nrow(x),ncol(x))
 x[p==0]=-Inf
 x[p==1]=Inf  
 return(x)
 }

