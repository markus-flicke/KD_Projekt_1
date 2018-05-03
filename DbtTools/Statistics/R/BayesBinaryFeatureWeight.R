 BayesBinaryFeatureWeight=function(BinaryVariable,Cls){
# BayesBinaryFeature(BinaryVariable,Cls)
# Classification weight for a binary classification 
# according  to Duda et al  Pattern Classification pp 52,53
#  w = log((p * (1-q)) / (q *(1-p)) )
# with  p = frequency of BinaryVariable==1 for Cls == c1
# and   q = frequency of BinaryVariable==1 for Cls == c2
#
# 
# INPUT
# BinaryVariable(1:n)      a binary variable consiting of only two values
#                          0 and 1, may contain NaN 
# Cls(1:n)                 a classification vector consiting of two classes
#                          c1 and c2, may  contain NaN
#
# OUPUT
# w           the weight of a decision using this variable
# p           frequency of BinaryVariable==1 for Cls == c1
# q           frequency of BinaryVariable==1 for Cls == c2
#author: MT 01/2016
if(length(Cls)!=length(BinaryVariable)) stop('unequal lengths of vectors')
# not defined for zero probabilities, replace with very small values
EPS = 1/10000
Vres=ClassCount(Cls)
uniqueClasses=Vres$UniqueClasses
numberOfClasses=Vres$NumberOfClasses
countPerClass=Vres$CountPerClass

if(numberOfClasses!=2) stop('Number of Classes have to be 2')

  ind1=which(Cls==uniqueClasses[1])
  pi=sum(BinaryVariable[ind1]==1,na.rm=T)/countPerClass[1]
  ind2=which(Cls==uniqueClasses[2])
  qi=sum(BinaryVariable[ind2]==1,na.rm=T)/countPerClass[2]
  
  nenner=qi*(1-pi)
  zaehler=pi*(1-qi)
  indeps=which(nenner<EPS)
  bruch=zaehler/nenner
  bruch[indeps]=NaN
  indeps2=which(bruch<indeps)
  wi=log(bruch)
  wi[indeps2]=NaN


return(list(w=wi,p=pi,q=qi))
       
}