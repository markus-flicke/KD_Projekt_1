RandIndex <- function (aCls, bCls) {
#RandIndex(aCls, bCls)
# Function calculates Rand index, adjusted Rand index, Mirkin index and hubert index 
# for two classifications of the same set of data
# INPUT
# aCls	Vector(1:n) of class identifiers 
# bCls	Vector(1:n) of class identifiers
# same index means same data in dataset
# OUTPUT
# a list with the following numbers:
# adjRandIndex=(A-nc)/(t1-nc);		%adjusted Rand - Hubert & Arabie 1985
# rawRandIndex=A/t1;			%Rand 1971		%Probability of agreement
# mirkinIndex =D/t1;			%Mirkin 1970	%p(disagreement)
# hubertIndex =(A-D)/t1;	    %Hubert 1977	%p(agree)-p(disagree)
# author: ?
# 1.Editor: MT 07/2015: RandIndex aus Fremdpaket uebernommen, da der Code hier falsch war
#requireRpackage('phyclust')
  requireNamespace('phyclust')
  rr = phyclust::RRand(aCls, bCls)
  
  cTable <- ContingencyTableSummary(aCls, bCls)
  c <- cTable
  n <- sum(sum(c))
  squaredRowSum <- sum(rowSums(c) ^ 2)
  squaredColumnSum <- sum(colSums(c) ^ 2)
  t1 <- choose(n, 2)
  t2 <- sum(c ^ 2)
  t3 <- 5 * (squaredRowSum + squaredColumnSum)
  
  nc <-
    (
      n * (n ^ 2 + 1) - ((n + 1) * squaredRowSum) - ((n + 1) * squaredColumnSum) +
        2 * (squaredRowSum * squaredColumnSum) / n
    ) / (2 * (n - 1))
  
  agreements <- (t1 + t2 - t3)
  disagreements <- (-t2 + t3)
  
  if (t1 == nc) {
    adjRandIndex <- 0
  }
  else {
    adjRandIndex <- (agreements - nc) / (t1 - nc)
  }
  
  rawRandIndex <- agreements / t1
  mirkinIndex <- disagreements / t1
  hubertIndex <- (agreements - disagreements) / t1
  
  return(
    list(
      adjRandIndex = rr$adjRand,
      rawRandIndex = rr$Rand,
      mirkinIndex = mirkinIndex,
      hubertIndex = hubertIndex,
      Eindex = rr$Eindex
    )
  )
}
# 
# [C,RowID,ColID,AclassCount,AClassFrequency,BclassCount,BClassFrequency]  = ContingencyTable(ACls,BCls);
# 
# n=sum(sum(C));
# nis=sum(sum(C,2).^2);		%sum of squares of sums of rows
# njs=sum(sum(C,1).^2);		%sum of squares of sums of columns
# 
# t1=nchoosek(n,2);		  %total number of pairs of entities
# t2=sum(sum(C.^2));	      %sum over rows & columnns of nij^2
# t3=.5*(nis+njs);
# 
# %Expected index (for adjustment)
# nc=(n*(n^2+1)-(n+1)*nis-(n+1)*njs+2*(nis*njs)/n)/(2*(n-1));
# 
# A=t1+t2-t3;		%no. agreements
# D=  -t2+t3;		%no. disagreements
# 
# if t1==nc
#    AdjRandIndex=0;			%avoid division by zero; if k=1, define Rand = 0
# else
#    AdjRandIndex=(A-nc)/(t1-nc);		%adjusted Rand - Hubert & Arabie 1985
# end
# 
# RawRandIndex=A/t1;			%Rand 1971		%Probability of agreement
# MirkinIndex =D/t1;			%Mirkin 1970	%p(disagreement)
# HubertIndex =(A-D)/t1;	    %Hubert 1977	%p(agree)-p(disagree)
# 