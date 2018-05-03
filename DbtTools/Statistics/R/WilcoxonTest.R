WilcoxonTest=function(Data,Cls) {
#WilcoxonTest(Data,Cls)
#Pvalues=V$Pvalues
#UniqueClasses=V$UniqueClasses)
# Wilcoxon rank sum test for equal medians , needs a binary Cls
#
# INPUT
# Data(d,n)         d cases,  n variables
# Cls(d)            Cls(i) == ClusterNumber of Data(i,:), Only the two smallest Numbers are used
#
# OUTPUT
# Pvalues(1:n)             Pvalues(i) is the p-value from the test 
# UniqueClasses(1:2)       the  two classes  used in  unique classes in Cls

# uses ranksum(x,y) == Matlabs Wilcoxon test
# MT 2016
if(is.matrix(Data))
  n=ncol(Data)
else
  n=1

Pvalues=c()

UniqueClasses = unique(Cls) 
AnzCls = length(UniqueClasses) 
if(AnzCls <2)     stop('WilcoxonTest: less than 2 classes in Cls')
if(AnzCls >2)   warning('WilcoxonTest: more than 2 classes in Cls') 


UniqueClasses =UniqueClasses[1:2]  # nur die ersten 2 Klassen werden benutzt

C1ind = which(Cls==UniqueClasses[1],arr.ind=T)  # index der 1. Klasse
C2ind = which(Cls==UniqueClasses[2],arr.ind=T)  # index der 2. Klasse

if(n>1){
  for(c in 1:n){
  x = noNaN(Data[C1ind,c])$elements
  y = noNaN(Data[C2ind,c])$elements
  p = wilcox.test(x,y,alternative="two.sided",paired=F)$p.value  # Matlabs Wilcoxon test
  Pvalues= c(Pvalues,p)        # ergebnis Uebertragen 
  }  # for c
}else{
  x = noNaN(Data[C1ind])$elements 
  y = noNaN(Data[C2ind])$elements
  p = wilcox.test(x,y,alternative="two.sided",paired=F)$p.value  # Matlabs Wilcoxon test
  Pvalues= c(Pvalues,p)   
}
return(list(Pvalues=Pvalues,UniqueClasses=UniqueClasses))
}