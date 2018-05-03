BestMatches2ProjectedPoints=function(Bestmatches,Lines){
  # XYcoords = BestMatches2ProjectedPoints(Data,Lines)
  # quadgrid coordinates (= BestMatchingUnit coordinates) to  a set of cartesian (x,y) coordinates
  #
  # 
  # INPUT
  # Bestmatches                     see package Umatrix 
  # OPTIONAL
  # Lines                           smaller Dimensions of overlying grid, default Lines= 50;
  #
  # OUTPUT
  # ProjectedPoints(1:n,1:2)        cartesian x,y coordinates
  #
  # author: MT 
  
  # eigentlich braucht man soviele Gitterpunkte, dass zwischen jedem
  # Datenpunkt auch noch ein Gitterpunkt liegt.
  n=nrow(Bestmatches)
  c=ncol(Bestmatches)
  if(c==2){
    pp=matrix(NaN,nrow=n,ncol=c)
    pp=Bestmatches[,c(2,1)]
  }else if(c==3){
    pp=matrix(NaN,nrow=n,ncol=(c-1))
    pp=Bestmatches[,c(3,2)]
  }else{
    stop('Error, wrong number of colums')
  }
  pp[,2]=Lines-pp[,2]
  return(ProjectedPoints=pp)
}