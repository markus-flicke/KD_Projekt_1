`sRelDiffTrans` <- function(x){

# function SRelDiff = SRelDiffTrans(x);
# % SRelDiff = sign(x) *RelDiffTrans(abs(x));
# % RelDiff fuer den Petrag von x, gut bei log-Vereilungen in +-
# %
# % INPUT
# % x(1:d,1:n)   Renditen = (B-A)/ A oder  Renditen =  Preis(t) -Preis(t-1) /Preis(t)
# 
# % OUTPUT
# % RelDiff(1:d,1:n)    RelDiff = Rendite/(0.5*Rendite +1) f¸r Rendite>-2 , NaN f¸r Rendite<= -2
# 
# % ALU Dez. 2007
# 
# SRelDiff = sign(x) .* RelDiffTrans(abs(x));
# 

	sRelDiff <- sign(x)*relDiffTrans(abs(x))
 	return (sRelDiff) 

}