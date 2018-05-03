`relDiff2Yield` <- function(relDiff){

# function Rendite = RelDiff2Rendite(RelDiff);
# % Rendite = RelDiff2Rendite(RelDiff);
# % Berechnet Reniten aus Reltiven Differenzen 
# % inverse Transormation zu RelDiffTrans, siehe dort
# 
# % INPUT
# % RelDiff(1:d,1:n)   Relative Differenz : RelDiff = Rendite/(0.5*Rendite +1)
# %                    RelDiff in [-2,2[,  RelDiff2Rendite(>=2) = NaN
# % OUTPUT
# % Rendite(1:d,1:n)   Renditen = (B-A)/ A oder  Renditen =  Preis(t) -Preis(t-1) /Preis(t)
# 
# % ALU Dez. 2007
# 
# RelDiff(find(RelDiff>=2)) = nan;
# Rendite = RelDiff ./ (1-0.5*RelDiff);

	relDiff[relDiff>=2] <- NaN
	yield = relDiff / (1-0.5 * relDiff)
	
	return(yield)
	

 }

