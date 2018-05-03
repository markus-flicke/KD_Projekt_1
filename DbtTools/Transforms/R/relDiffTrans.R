`relDiffTrans` <- function(yield){

# function RelDiff = RelDiffTrans(Rendite);
# % RelDiff = RelDiffTrans(Rendite);
# % RelDiff = RelDiffTrans(Prozent/100);
# % Uebersetzung von Renditen oder Prozentwerten = Renditen/100 in Relative Differenzen
# % Rendite = (B-A)/ A oder  Rendite =  Preis(t) -Preis(t-1) /Preis(t)
# % 
# % 	Die RelDiffTransformation ist eine stetige und monotone Funktion von Rendite bzw. LogRatio (LogRendite)
# %  	RelativeDifferenzen kˆnnen wieder in Renditen bzw. LogRatios umgerechnet werden.
# %  	Der Ruin kann im Gegensatz zu LogRatio (LogRendite) dargestellt werden. Bei Ruin gilt RelDiff = -2
# %  	Exorbitante Zuwachsraten f¸hren zu RelDiff -> 2
# %  	Begrenzter und symmetrischer Werebereich: [-2, 2].
# %  	Im Bereich  [-25% , 25%] ist RelDiff identisch mit der Rendite.
# %  	Im Bereich  1/2.5  bis 2.5  ist RelDiff  identisch mit LogRendite (LogRatio).
# %  	Die RelativeDifferenz ist inhaltlich leicht zu interpretieren: 
# %   die RelativeDifferenz ist die auf den mittleren Grundwert bezogene ƒnderung.
# %
# % inverse Trnfsformation ist RelDiff2Rendite(RelDiff)
# %
# % INPUT
# % Rendite(1:d,1:n)   Renditen = (B-A)/ A oder  Renditen =  Preis(t) -Preis(t-1) /Preis(t)
# %                    ¸blicherweise Rendite(:) >-1 .
# %                    f¸r Renditen <= -2  RelDiff = NaN
# 
# % OUTPUT
# % RelDiff(1:d,1:n)    RelDiff = Rendite/(0.5*Rendite +1) f¸r Rendite>-2 , NaN f¸r Rendite<= -2
# 
# % ALU Dez. 2007
# 


# Rendite(find(Rendite<=-2)) = nan;
	yield[yield<= -2] <- NaN
# RelDiff = Rendite./(0.5*Rendite +1); % 
	relDiff <- yield/(0.5*yield+1)


 return (relDiff) 

}