precisionRecall=function(retrieved, relevant){
# V <- precisionRecall(retrieved, relevant)
# Precision <- V$Precision
# Recall <- V$Recall
#
# Berechnet Precision und Recall des Information Retrieval.
#
# INPUT:
# retrieved[1:m]       vector of set of retrieved items. (e.g. the list of objects produced) 
#												(CL: Terms from second DAG with smaller gene set)
# relevant[1:n]        vector of all objects that are relevant for a certain topic  
#												(CL: Terms from original DAG with original gene set)   
# 
# OUTPUT:
# Precision            the fraction of retrieved objects that are relevant to the find
#           
# Recall               fraction of the objects that are relevant to the query that 
#                      are successfully retrieved
  
# Autor: MT
# 1. Editor: CL -"betha" entfernt, da es in der Berechnung eh nicht beruecksichtigt wird.
#								- Funktion umbenannt, "IR." gestrichen, s.d. Fktname = Filename.
#

## Precision und Recall ##
Schnitt <- intersect(retrieved, relevant) # Zaehler sowohl fuer Precision als auch Recall

r <- length(Schnitt)/length(relevant)
p <- length(Schnitt)/length(retrieved)

return(list(Precision=p,Recall=r))
}# end function precisionRecall