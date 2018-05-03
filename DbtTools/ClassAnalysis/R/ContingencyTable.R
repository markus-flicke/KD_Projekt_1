ContingencyTable <- function(RowCls,ColCls) {
# list(Ctab,RowID,ColID,RowClassCount,RowClassPercentages,ColClassCount,ColClassPercentages)  = ContigencyTable(RowCls,ColCls);
# contingency tabe of two Cls
#
# INPUT
# RowCls,bCls                          vector of class identifiers (i.e.integers or NaN's) of the same length
                  
# OUTPUT list with this elements:
# cTab                                 cTab(i,j) contains the count of all Instances where
#                                     the i-th class in RowCls equals the j-th class inColCls
# rowID                               the different classes in RowCls, corresponding to the rows of cTab  
# colID                               the different classes inColCls, corresponding to the columns of cTab 
# RowClassCount,RowClassPercentages   instance count  and precentages of classes in RowCls sorted according rowID
# ColClassCount,ColClassPercentages   instance count  and precentages of classes inColCls sorted according colID
  
# ALU normalisierung Ausbauen!
# flache tabelle zureuckliefern fuer Komfortable tabellen siehe ContingencyTableSummary()
warning('Not validated! Please use ContingencyTableSummary()')
RowCounts = ClassCount(RowCls);
ColCounts = ClassCount(ColCls);
  
# aNormalized <- NormalizeCls(RowCls)
# rowID <- aNormalized[[3]]
# aNormalized <- aNormalized[[1]]
aNormalized <- RowCls; # keine Normalisierung
rowID               <- RowCounts$uniqueClasses
RowClassCount       <- RowCounts$countPerClass
RowClassPercentages <- RowCounts$classPercentages

# bNormalized <- NormalizeCls(bCls)
# colID <- bNormalized[[3]]
# bNormalized <- bNormalized[[1]]
bNormalized <- ColCls# keine Normalisierung
colID               <- ColCounts$uniqueClasses
ColClassCount       <- ColCounts$countPerClass
ColClassPercentages <- ColCounts$classPercentages

cTab <- table(RowCls, ColCls) # faulheit !
Table=ftable(cTab)


return(list(cTab = Table, rowID = rowID, colID = colID, RowClassCount = RowClassCount, RowClassPercentages = RowClassPercentages, ColClassCount = ColClassCount, ColClassPercentages = ColClassPercentages))
}