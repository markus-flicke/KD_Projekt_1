`relDiff4Percent` <- function(percent){

# function RelDiff = RelDiff4Prozent(Prozent);
# % RelDiff = RelDiff4Prozent(Prozent);
# % Prozent in [-100,inf] = Wert(heute)-Wert(gestern)/Wert(gestern)*100
# % Uebersetzung von Prozenten  in Relative Differenzen
# % INPUT
# % Prozent(1:d,1:n)     Prozent in [-100,inf] = Wert(heute)-Wert(gestern)/Wert(gestern)*100
# % OUTPUT
# % RelDiff(1:d,1:n)    RelDiff = Rendite ./(0.5*(Rendite) +1) wobei  Rendite =Prozent/100;% 
# 
# % ALU Dez. 2007
# 


# Rendite =Prozent/100;

	yield <- percent/100
# Nenner = (0.5*(Rendite) +1);
	divider <- ((0.5*yield)+1)
# RelDiff= Prozent*0; % Init to Zero
	relDiff <- percent*0
# NoNzeroInd=find(abs(Nenner)>10^(-9)); % aller nichtnull Nenner
	nonZeroInd<-(abs(divider)>10^(-9))
# RelDiff(NoNzeroInd) = Rendite(NoNzeroInd) ./Nenner(NoNzeroInd);
	relDiff[nonZeroInd] <- yield[nonZeroInd] / divider[nonZeroInd]
# 
# 

 return (relDiff) 

 }

