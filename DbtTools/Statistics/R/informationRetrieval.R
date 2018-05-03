informationRetrieval=function(retrieved,relevant,betha=0.5){
# werte=informationRetrieval(TermCurt,TermFull,pval)
# Bei gegebener Referenzliste Full und Vergleichliste Curt werden die Variablen des Information Retrievals berechnet
#
# INPUT
# vCurt[1:m]       vector, 
# vFull[1:n]       vector, 
#
# OUTPUT
# Precision           vector
# Recall              vector
# AUC                 vector, AREA of Recall and Precision          
# Fbetha              vector, special case of harmonic mean known as weighted F measure or F score  
# Autor: MT
# 1. Editor:
#
# EXAMPLE
# 
# Nota  
rundp=IR.PrecisionRecall(retrieved,relevant)
aundf=IR.AUCandFbetha(rundp$Precision,rundp$Recall,betha)
return(list(Precision=rundp$Precision,Recall=rundp$Recall,AUC=aundf$AUC,Fbetha=aundf$Fbetha))
}