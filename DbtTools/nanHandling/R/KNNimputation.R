KNNimputation <- function(Data,k=3){
# ImputedData = KNNimputation(Data,k,defined);
# replace NaN in Data with average valus of the k Nearest Neigbours in Data
#
# INPUT
# Data                   Data set some of which contains NaN, a nonempty subset is  complete, [1:N,1:d]

# OPTIONAL
#  k                     k>=1 number of nearest neigbors to use default =3;

# Author: 05/15 RG, imported from matlab

if(is.vector(Data))
  stop("Wrong dimension of Data.")
  
  
AnzData = nrow(Data)
d = ncol(Data)

k = max(1,k)  # K muss mindestens ==1 sein

ImputedData  = Data # init outout
AnzNaNs = zeros(AnzData,d)

# nur fuer die daten, die NaN enthalten eine Ersetzung vornehmen;
dummy <- apply(Data, 1, function(x)!any(is.na(x)))
HasNaNind = which(dummy==FALSE)
PureInd   = which(dummy==TRUE)

AnzPure   = length(PureInd);

if(AnzPure <1)
  stop("KNNimputation: there is no case in Data which is complete")

PureData = Data[PureInd,]
#PureData <- Data[apply(Data, 1, function(x)!any(is.na(x))), , drop=F]

AnzNaNdata = length(HasNaNind);
for(i in 1:AnzNaNdata){
  n = HasNaNind[i]
  x = Data[n,]             # x == der datensatz der NaN enhaelt
  NoNanColum = !(is.nan(x))                       # die Spalten die zu ersezten sind, nur diese bei distanz bereucksichtigen
  DistToAll = t(dist2All(x,PureData,NoNanColum,'euclid')) # die Distanz von x zu allen in Pure Data
  Sorted <- sort(na.last=NA,DistToAll,index.return = TRUE)
  DistToAll = matrix(Sorted$x)
  Sind = matrix(Sorted$ix)               # distanzen aufsteigend sortieren 
  KNNmean = colMeans(PureData[Sind[1:k],]);           # mittelswert der  k am naechsten liegenden Daten
  NaNsInd = which(is.nan(x), arr.ind=TRUE);                         # indices der Werte die NaN Enthalten
  ImputedData[n,NaNsInd] = KNNmean[NaNsInd];         # diese Werte werden ersetzt
}

return(ImputedData=ImputedData)}