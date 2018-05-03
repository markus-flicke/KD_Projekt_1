`relDiff4Yield` <- function(yields){

# function RelDiff = RelDiff4Rendite(Rendite);
# % RelDiff = RelDiff4Rendite(Rendite);
# % Uebersetzung von Renditen  in Relative Differenzen
# % INPUT
# % Rendite(1:d,1:n)    Renditen, nicht als % !
# % OUTPUT
# % RelDiff(1:d,1:n)    RelDiff = Rendite ./(0.5*(Rendite) +1); % 
# 
# % ALU Dez. 2007
# 
# 
# RelDiff = Rendite ./(0.5*(Rendite) +1); % 
# 

 return (yields/(0.5*(yields)+1)) 

 }

