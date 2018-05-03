ProjectedPoints2Bestmatches=function(ProjectedPoints,Lines){
  n=nrow(ProjectedPoints)
  c=ncol(ProjectedPoints)
  if(c==2){
    pp=matrix(NaN,nrow=n,ncol=c)
    pp=ProjectedPoints[,c(2,1)]
  }else if(c==3){
    pp=matrix(NaN,nrow=n,ncol=(c-1))
    pp=ProjectedPoints[,c(3,2)]
  }else{
    stop('Error, wrong number of colums')
  }
  pp[,1]=Lines-pp[,1]+1
  return(Bestmatches=pp)
}