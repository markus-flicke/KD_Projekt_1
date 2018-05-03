SignificantContingencyTable=function(Xtab,ErrorNiveau=0.05){
# res = SignificantContingencyTable(Xtab,ErrorNiveau)
# see if actual clusters are significant differnt from expected 
# uses [Lower, Upper] = binomialQuantiles(ns, p, alpha);
#
# INPUT   
# Xtab(1:l+2,1:c+2)             a crosstabulation produced for example by ContingencyTableSummary()
#
# OPTIONAL
# ErrorNiveau      error niveau typically 0.05 = 5#
  #
# OUTPUT
# SignificantRelDiff      		Relative Differences, that are Significant  in #
  # ExpectedXtab(1:l,1:c)            the Xtab of expected values
# IsSignificant(i,j)      the given number of members of class i differs
#                          significantly from the expected number for class j
# IsSignificant(i,j)==1    the number is too high
# IsSignificant(i,j)==-1   the number is too low
# author: MT 07/2015
  
rc=dim(Xtab)
AnzRows=rc[1]
AnzCols=rc[2]

#if(AnzCols==2){ # Xtab== [RowCls,ColCls]
#RowCls=Xtab(:,1);
#ColCls=Xtab(:,2);
#Xtab = ContingencyTable(RowCls,ColCls);
#} if AnzCols==2 # Xtab== [RowCls,ColCls]

#[TabRows,TabCols] = size(Xtab);
TabRows=AnzRows-2
TabCols=AnzCols-2
ClassTab = Xtab[1:TabRows,1:TabCols];
#TotalInRow = Xtab[1:TabRows,TabCols+1]
TotalInRow=Xtab[TabRows+1,1:TabCols]
PriorNrInClasses = TotalInRow
NrData = sum(PriorNrInClasses)
if(NrData==0) stop('SignificantContingencyTable, Xtab empty!')

PriorProbability = PriorNrInClasses/NrData;
GivenNrInClass = Xtab[1:TabRows,TabCols+1]
ExpectedXtab = GivenNrInClass %*%t( PriorProbability)

rc=dim(ExpectedXtab)
ExpRows=rc[1]
ExpCols=rc[2]
LowLimit = ExpectedXtab*0; HiLimit =ExpectedXtab*0; # INIT
for(r in 1:ExpRows){
    for(c in 1:ExpCols){
        PriorProb = PriorProbability[c]
        ObservedNumber = ClassTab[r,c]
        blist=binomialQuantiles(GivenNrInClass[r], PriorProb, ErrorNiveau)
        LowLimitPercent=blist$lowerPercent
        HiLimitPercent =  blist$upperPercent
        LowLimit[r,c]= LowLimitPercent*GivenNrInClass[r]
        HiLimit[r,c]= HiLimitPercent*GivenNrInClass[r]
  }# for c 
}#for r 
IsSignificantLow = ClassTab <LowLimit
IsSignificantHi = ClassTab >HiLimit

IsSignificant= IsSignificantHi-IsSignificantLow

# calculate Relative Differences and set insignificant to NaN
SignificantRelDiff=ClassTab*0; # init;
Nenner = (ClassTab+ExpectedXtab)
#Ind =which(Nenner>0.000000001,arr.ind=T); # avid zero division
#Ind=Nenner==0
Null=round(Nenner,7)==0
if(sum(Null)==0){
  SignificantRelDiff= (ClassTab-ExpectedXtab)/Nenner*200;
}else{
  SignificantRelDiff= (ClassTab-ExpectedXtab)/Nenner*200;
  warning('Nenner ist Teilweise Null')
}
return(list(SignificantRelDiff=SignificantRelDiff,ExpectedXtab=ExpectedXtab,IsSignificant=IsSignificant))
}