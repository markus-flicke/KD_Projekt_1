`yields4Prices` <- function(prices,minPrice=0.01){
	
# function [Renditen,MittlererPreis] = Renditen4Preise(Preise,MinPreis);
# % Renditen = Rentiten4Preise(Preise,MinPreis);
# % Renditen =  (Preis(heute)-Preis(gestern))/Preis(gestern)
# % INPUT
# % Preise(1:AnzTage,1:AnzKurse)  Preise tegeweise in den Spalten. 
# % OPTIONAL
# % MinPreis       Preise = max(Preise,MinPreis), default MinPreis = 0.01
# % OUTPUT
# % Renditen(1:AnzTage,1:AnzKurse)       = (Preise-GestrigePreise)./GestrigePreise;
# % MittlererPreis(1:AnzTage,1:AnzKurse) = 0.5*(Preise+GestrigePreise);
# 
# % ALU Dez. 2007
# if nargin < 2 ; MinPreis = 0.01; end; % minimaler Aktienpreis = 1 cent
# 
# [AnzTage,AnzKurse] = size(Preise);
	nDays = dim(prices)[1]
	nQuotations = dim(prices)[2]


# Gestern = [1 1:(AnzTage-1)]';
	yesterday = c(1,1:(nDays-1))   # <------ Transformieren?

# GestrigePreise = Preise(Gestern,:);
	yPrices = prices[yesterday,]
# 
# GesternZeroInd = find(GestrigePreise<MinPreis);
# GestrigePreise(GesternZeroInd) = nan; % underflow verhindern
	yPrices[yPrices<minPrice]=NaN;
	 
# Renditen  = (Preise-GestrigePreise)./GestrigePreise;
	yields <- (prices-yPrices)/yPrices
# 
# MittlererPreis = 0.5*(Preise+GestrigePreise);
	aPrice <- 0.5*(prices+yPrices)

 return (list(yield=yields,averagePrices=aPrice)) 
 
}