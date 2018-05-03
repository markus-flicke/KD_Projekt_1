ContingencyTableSummary <- function(RowCls, ColCls) {
# Xtable  = ContingencyTableSummary(RowCls,ColCls);
# comfortable crosstabulation of instances in two different classifications
# including class headers, counts & frequencies
# uses MATLAB's function crosstab
# INPUT
# RowCls,ColCls                    vector of class identifiers (i.e.integers or NaN's) of the same length
#                   
# OUTPUT 
# Xtable                        die Kontingenz tabelle erganzt um Klassennamen, Summen und Percentages fuer Zeilen und Spalten 
# author: MT 07/2015  
#C <- ContingencyTable(RowCls, ColCls)

  RowID=length(unique(RowCls))
  ColID=length(unique(ColCls))
  Ctable=table(RowCls, ColCls)
  
AllinTab = sum(Ctable)

ColumnSum = colSums(Ctable)
ColPercentage = round(ColumnSum/AllinTab*100,2)

RowSum    = rowSums(Ctable)
RowPercentage = round(RowSum/AllinTab*100,2)

Rows <- rbind(round(Ctable), ColumnSum,ColPercentage )
Xtable <- cbind(Rows,c(RowSum,AllinTab,0),c(RowPercentage,0,100))

colnames(Xtable)=c(1:ColID,'RowSum','RowPercentage')

return(Xtable) 
}