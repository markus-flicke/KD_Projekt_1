`relDiff4LogRatio` <-
function(logRatio){

# function RelDiff = RelDiff4LogRatio(LogRatio);
# % RelDiff = RelDiffTrans(LogRatio);
# % Uebersetzung von LogRatio  in Relative Differenzen
# % Achtung: gilt nur naeherungsweise! gut fuer LogRatio in [-0.5 0.5]
# % INPUT
# % LogRatio(1:d,1:n)  
# % OUTPUT
# % RelDiff(1:d,1:n)    RelDiff = exp(LogRatio)-1./(0.5*(LogRatio +1)); 
# 
# % ALU Dez. 2007
# 
# RelDiff = (exp(LogRatio)-1) ./(0.5*(exp(LogRatio) +1)); % 
# 

 
 relDiff <- (exp(logRatio)-1) / (0.5*(exp(logRatio)+1))
 return(relDiff)

 }

