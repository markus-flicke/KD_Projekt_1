IR.PrecisionRecall=function(retrieved,relevant,betha=0.5){
# werte=IR.PrecisionRecall(retrieved,relevant)
# Berechnet gaengige Variablen des Information Retrieval
#
# INPUT
# retrieved[1:m]       vector or list, set of retrieved  (e.g. the list of objects produced) 
# relevant[1:n]        vector of all objects that are relevant for a certain topic     
# 
# OPTIONAK
# betha                paramter for Fbetha, default=0.5 ## CL: Wird im Code nicht verwendet!
#
# OUTPUT
# Precision            the fraction of retrieved objects that are relevant to the find
#           
# Recall               fraction of the objects that are relevant to the query that 
#                      are successfully retrieved
  
# Autor: MT
# 1. Editor: CL: Programmname passend zu Funktionsname umbenannt.
#
# EXAMPLE
# 
# Nota
## Precision und Recall ##
if(is.list(retrieved)){retrieved=unlist(retrieved)}

res=intersect(retrieved,relevant)
length(res)/length(relevant)

#P&R bei pValue-Schranke = 1
r=length(res)/length(relevant)
p=length(res)/length(retrieved)

return(list(Precision=p,Recall=r))
  
}