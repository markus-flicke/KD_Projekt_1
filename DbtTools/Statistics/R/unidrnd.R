unidrnd=function(N,m,n=1){  
# unidrnd(Lines,AnzData,1)
# generates random numbers for the discrete uniform distribution with maximum N.
#  INPUT
#  N            maximum
#  [m,n]        Dimensionen der AusgabeMatrix
#  
#  
#  OUTPUT
#  R[m,n]            Discrete uniform random numbers vector          
# 
#  NOTE: any errors calling kmeans may be due to a kmeans routine in the SOM pack (Vesanto)
#  check the path priority using which kmeans result must be: ...\MATLAB\toolbox\stats\kmeans.m
#   
# Author: MT, reimplemented from matlabs  unidrnd
  
 if(n==1){
 R=ceiling(runif(m, min=0, max=N)) # values between 1 and N

 }else{
		R=c()
      for(i in 1:n){
          R=rbind(R,unidrnd(N,m))
      }
 }
 return(R)
}