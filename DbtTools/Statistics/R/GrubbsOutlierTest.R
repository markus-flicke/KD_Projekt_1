GrubbsOutlierTest=function(X){
# res= GrubbsOutlierTest(X)
# performs an Grubbs test for identifying outliers 
# returns the possible Outliers and the corresponding Pvalues
#
# INPUT
# X(1:n)                  data,  may contain NaN
#
# OUTPUT Liste res
# Outlier(1:o)            possible outliers in X
# X(1:n1)                 a sorted and cleaned version of input X
# Pvalues X(1:n1)         P-values of a one-sided Grubbs test for outliers     
#                         the values correspond to the output X 
# OutlierInd              Indize in orignal Ix of Outlier
# NotOutlierInd           restliche Indizes
  
# MT 08/2015 nach matlab: ALU Aug. 2015


# Method:
# G = (X -Mean(X))/StdX
# sollte t-verteilt sein zum freiheitsgrad n -2 wobei n = length(X):
Xtmp=X
X = as.vector(X) # make it a column vector
X =  noNaN(X)$elements   # nur die Nicht Nan daten werden betrachtet
MeanX = mean(X)
StdX  = sd(X)
n = length(X)

ValidX = (n >6) & (StdX >0)

# grubbs statistik

G = (X -MeanX)/StdX
# sortieren nach den groessten G werten

Vsort= sortdescending(abs(G))
G=Vsort$sort
Sind=Vsort$indices

X = X[Sind]


IsOutlier = X*0
Pvalues   = IsOutlier+1
N = n
n2 = N-2
Pval = 1-pt(G,n2)  # Tstatistik rechen 
LargestIsOutlier = Pval[1]<0.05
i= 1
IsOutlier[i] = LargestIsOutlier
Pvalues[i]   = Pval[1] 
while (LargestIsOutlier & (N >10)){
    G = G[2:length(G)]                  # den outlier rausnehmen und alles neu rechnen
    N = N-1;                      # Anzahl Daten hat sich vermindert
    n2 = N-2
    Pval = 1-pt(G,n2);           # neue t- statistik
    LargestIsOutlier = Pval[1]<0.05
    i=i+1;
    IsOutlier[i] = LargestIsOutlier
    Pvalues[i]   = Pval[1]
}
# jetzt sind alle vermutlichen outlier gefunden oder es gibt nur noch 10 Datensaetze
Outlier = X[IsOutlier>0]
ind=c()
if(length(Outlier)>0){
  for(i in 1:length(Outlier)){
    ind=c(ind,which(Xtmp==Outlier[i]))
  }
  NotOutlierInd=setdiff(c(1:length(X)),ind)
}
return(list(Outlier=Outlier,X=X,Pvalues=Pvalues,OutlierInd=ind,NotOutlierInd=NotOutlierInd))
}