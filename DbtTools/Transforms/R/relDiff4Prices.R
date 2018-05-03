relDiff4Prices <- function(prices, minPrice=0.01){
# RelDiff <- relDiff4Prices(Preise,MinPreis);
# Transformation  von Preise  in Relative Differenzen
# INPUT
# prices(1:AnzTage,1:AnzKurse)  Preise tageweise in den Spalten.
# 
# OPTIONAL
# minPrice       Preise = max(Preise,MinPreis), default MinPreis = 0.01
# 
# OUTPUT
# RelDiff(1:AnzTage,1:AnzKurse)  
 
# ALU 2014
  
# [AnzTage,AnzKurse] = size(Preise);
  S = size(prices);
	nDays <-S[1]
	nQuotations = S[2]

# Gestern = [1 1:(AnzTage-1)]';
	yesterday = c(1,1:(nDays-1)) # index von Gestern

# GestrigePreise = Preise(Gestern,:);
 if (nQuotations>1){
	 yPrices = prices[yesterday,]
 }else{
   yPrices = prices[yesterday];
 } # end  if (nQuotations>1)

# 
# Nenner  = (Preise+GestrigePreise);
	deno <- prices+yPrices

# NennerZeroInd = find(Nenner<MinPreis);
# Nenner(NennerZeroInd) = nan;  % underflow verhindern
	deno[deno<minPrice]<-NaN;

# 
# RelDiff = 2* (Preise - GestrigePreise)./Nenner;

	relDiff <- 2*(prices - yPrices) / deno
 	return(relDiff)

 }

