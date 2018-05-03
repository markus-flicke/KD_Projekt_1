`logRatio4Prices` <- function(prices, minPrice = 0.01){

# function LogRenditen = LogRatio4Preise(Preise,MinPreis);
# % LogRenditen = LogRatio4Preise(Preise);
# % Uebersetzung von Preisen  in LogRatios 
# % LogRendite = log(Preis(heute)/Preis(gestern))
# % NOTA: log(Rendite(heute) +100%) =  log(Preis(heute)/Preis(gestern)) f¸r Renditen < 10%
# % INPUT
# % Preise(1:AnzTage,1:AnzKurse)  Preise tegeweise in den Spalten. 
# % OPTIONAL
# % MinPreis       Preise = max(Preise,MinPreis), default MinPreis = 0.01
# % OUTPUT
# % LogRenditen(1:AnzTage,1:AnzKurse)  
# 
# % ALU Dez. 2007



nDays = dim(prices)[1]
nQuotations = dim(prices)[2]


# Gestern = [1 1:(AnzTage-1)]';
yesterday = c(1,1:(nDays-1))   # <------ Transformieren?
 
# GestrigePreise = Preise(Gestern,:);
yPrices = prices[yesterday,]



# GesternZeroInd = find(GestrigePreise<MinPreis);
# GestrigePreise(GesternZeroInd) = nan; % underflow verhindern
yPrices[yPrices<minPrice]=NaN;


# HeuteZeroInd   = find(Preise<MinPreis);
# Preise(HeuteZeroInd) = nan; % underflow verhindern
prices[prices<minPrice]=NaN;

# LogRenditen  = log(Preise)-log(GestrigePreise);


logYield = log(prices)-log(yPrices) ########### ??????????????


return(logYield)

}