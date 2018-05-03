Chi2SignificantContingencyTable=function(Xtab){
# [Pvalue,ExpectedXtab,SquaredDiffs,SumSquaredDiffs] = Chi2SignificantContingencyTable(Xtab);
# Uses Chi2Test to see whether the Independence assumption is valid
# see also: [table,chi2,p] = crosstab(col1,col2)
#
#
# INPUT   
# Xtab(1:l+2,1:c+2)             a crosstabulation produced for example by ContingencyTableSummary
#
# OUTPUT
# Pvalue                           the Pvalue of the chi2 test
# ExpectedXtab(1:l,1:c)            the Xtab of expected values = product of the frequencies * Total number
# SquaredDiffs(1:l,1:c)            SquaredDiffs(r,c) = (ObservedNumber- ExpectedNumber)/ ExpectedNumber;
# SumSquaredDiffs                  = nansum(SquaredDiffs(:));

# Mt 08/2015
rc=dim(Xtab)
AnzRows=rc[1]
AnzCols=rc[2]
TabRows=AnzRows-2
TabCols=AnzCols-2
ClassTab = Xtab[1:TabRows,1:TabCols];

TotalAnz = sum(ClassTab, na.rm = T)
if(TotalAnz<1) stop('Chi2SignificantContingencyTable: Xtab empty!')
AnzRows=TabRows
AnzCols=TabCols
  
AnzInCol = colSums(ClassTab, na.rm = T)
AnzInRow = rowSums(ClassTab, na.rm = T)

ColFreq  = rep(1,AnzRows)*AnzInCol/TotalAnz
RowFreq  = AnzInRow*rep(1,AnzCols)/TotalAnz
ExpectedXtab =t(TotalAnz*ColFreq%*%t(RowFreq))

SquaredDiffs = (ClassTab- ExpectedXtab)^2 / ExpectedXtab
SumSquaredDiffs = sum(SquaredDiffs, na.rm = T)

Df = (AnzRows-1)*(AnzCols-1) # Freiheitsgrad der Chi2 Verteilung: produkt aus Zeilenanz-1 und Spaltenanz-1

Pvalue= 1-pchisq(SumSquaredDiffs,Df)
return(list(Pvalue=Pvalue,ExpectedXtab=ExpectedXtab,SquaredDiffs=SquaredDiffs,SumSquaredDiffs=SumSquaredDiffs))
}