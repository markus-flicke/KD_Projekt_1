IR.AUCandFbetha=function(Precision,Recall,betha=0.5){
# werte=IR.AUCandFbetha(retrieved,relevant)
# Berechnet gaengige Variablen des Information Retriveal
#
# INPUT
# Precision[1:m]       vector, positive predictive value) is the fraction of retrieved instances that are relevant
# Recall[1:n]          vector, is the fraction of relevant instances that are retrieved, known as sensitivity
# 
# OPTIONAl
# betha                paramter for Fbetha, default=0.5
#
# OUTPUT
# AUC                 vector, AREA of Recall and Precision
#           
# Fbetha              vector, special case of harmonic mean known as weighted F measure or F score 

# Autor: MT
# 1. Editor:
#
# EXAMPLE
# 
# Nota

  fb=(1+betha^2)*Precision*Recall/(betha^2*Precision+Recall)
  
  ## Berechne AUC ##
  a = (Precision*Recall)
  return(list(AUC=a,Fbetha=fb))
}