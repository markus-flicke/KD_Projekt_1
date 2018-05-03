EffectSize=function(Data,Cls,IsParametric=F) {
# V=EffectSize(Data,Cls,IsParametric=F)
# CohensD=V$CohensD
#  Cls1=V$Cls1
#  Cls2=V$Cls2
#  Diff=V$Diff
# EffectSize of a binary classification
# equals Cohen's d for parametric (Gaussian) distributions
# for nongaussian distribitions an analogous formula using medians and
# Adjuested MAD (Median absolute differences) is used
#
# INPUT
# Data(n,d)           n cases,  d variables
# Cls(n)              cls(i) == ClusterNumber of data(i,:)
#                     NOTE it is assumed that the classificaion is Binary i.e. contains only two classes: Cls1 and Cls2
#                     if there are more than two classes only two of them are used
#
# OPTIONAL
# IsParametric        ==1 => Cohen's d is calculated, otherwise medians are used, default: IsParametric==0
#
# OUTPUT
# CohensD(1:d)       = Diff / PooledDev
# Cls1,Cls2           the numbers of the two binary Classes in Cls which are used
# 
# Diff(1:d)      =    ClassMeanRobust(D(:,Cls2)) -  ClassMeanRobust(D(:,Cls1)),iff IsParametric ==1
# Diff(1:d)      =    ClassMedian(D(:,Cls2)) -          ClassMedian(D(:,Cls1)),iff IsParametric ~=1
#
# PooledDev(1:d) = sqrt((n1-1)*Dev1^2+(n2-1)*Dev2^2/(n-2))  where n1 is the number of elements (notNaN) in Cls1 , n2 analog
#
# Dev1(1:d)      = ClassStdRobust(D(Cls1,:))  i.e. the (robust) standard deviation of Cls1 ignoring nan , iff IsParametric ==1
# Dev2(1:d)      = ClassStdRobust(D(Cls2,:))  i.e. the (robust) standard deviation of Cls2 ignoring nan , iff IsParametric ==1
# 
# Dev1(1:d)      =  ClassAMAD(D(Cls1,:))  i.e. the adjusted MAD  of Cls1 ignoring nan , iff IsParametric ~=1
# Dev2(1:d)      =  ClassAMAD(D(Cls2,:))  i.e. the adjusted MAD  of Cls2 ignoring nan , iff IsParametric ~=1
#
# n1(1:d),n2(1:d)   the number of notNaN elements in the classes


NotNan = !is.nan(Data)
V=  ClassSum(NotNan,Cls)   # die nicht NaNs in jeder Klasse zählen, 
UniqueClasses=V$UniqueClasses
N=V$SumPerClass

Cls1=UniqueClasses[1]    # die Klasse 1
Cls2=UniqueClasses[2]    # die Klasse 2

n1 = N[1,]           # die nicht NaNs in  Cls1        
n2 = N[2,]           # die nicht NaNs in  Cls2

if(IsParametric){  # Gaussian assumption claclulate Cohen's D
V = ClassMeanRobust(Data,Cls) 
UniqueClasses=V$UniqueClasses
MPerClass=V$MeanPerClass  
V  =  ClassStdRobust(Data,Cls) 
UniqueClasses=V$UniqueClasses
SPerClass=V$StdrobustPerClass
}else{ # nichtparametrisch, use median and AMAD
V  = ClassMedian(Data,Cls) 
UniqueClasses=V$UniqueClasses
MPerClass=V$MedianPerClass
V  =   ClassAMAD(Data,Cls) 
UniqueClasses=V$UniqueClasses
SPerClass=V$AMADPerClass 
}   #    IsParametric== 1 

Diff = MPerClass[2,] - MPerClass[1,] 
Dev1 = SPerClass[1,] 
Dev2 = SPerClass[2,] 

PooledDev =   sqrt(((n1-1)*Dev1^2 + (n2-1)*Dev2^2 )/(n1+n2-2)) 

CohensD   =   -Diff /PooledDev 
return( list(CohensD=CohensD,Cls1=Cls1,Cls2=Cls2,Diff=Diff, PooledDev=PooledDev,Dev1=Dev1,Dev2=Dev2,n1=n1,n2=n2))
 }