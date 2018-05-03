spy <- function (M) {
# spy(M) 
# plots the sparsity pattern of a  matrix M.
#
# INPUT
# S(m,n)              a mxn Matrix, the nonzero elements are plotted
#
#      uses   PlotPixMatrix(M,Names,LowLim,HiLim) 

# ALU

  Names =NULL;
  LowLim=0;       
  NZInd = which(M>0);
  if (length(NZInd) >0){
     HiLim = min(M[NZInd]);
     PlotPixMatrix(M,Names,LowLim,HiLim) ;
  }else {
     print(paste('spy(): nichts zu zeichnen, M= ', M)) # do nothing
  } # end if (length(NZInd) >0)     
 
}  #end   function  spy(M)

