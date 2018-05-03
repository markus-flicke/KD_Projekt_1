OptNrOfBins <-
function(Data){

# function [OptimalNrOfBins] = OptNrOfBins(data); 
# % [OptimalNrOfBins] = OptNrOfBins(data)
# %
# % DESCRIPTION
# % Berechung der optimalen Anzahl von Bins fuer ein Histogramm
# % nach Keating/Scott 99
# %
# % INPUT
# % data               die Daten
# %
# % OUTPUT
# % OptimalNrOfBins   die bestmoegliche ANzahl von Bins, minimal jedoch 10
# %                   Verwendung fuer hist(data,OptimalNrOfBins);
# 
# AnzInData = sum(~isnan(data));% Anzahl Daten ohne NaN
# if AnzInData <1  % kein Datenpunkt vorhanden
#     OptimalNrOfBins = 0;
#     return ;
# end;
# 
# Sigma = nanstd(data); % Std. dev ohne NaN
# %p = percentiles(data); % Perzentile feststellen
# %InterQuartileRange = p(75) - p(25); % InterQuartils-Range
# p = prctile(data, [25 75]); % Perzentile feststellen
# InterQuartileRange = p(2) - p(1); % InterQuartils-Range
# 
# SigmaDach= min(Sigma, InterQuartileRange/1.349)  ;       % see Keating/Scott 99
# OptimalBinWidth = 3.49 * SigmaDach / (AnzInData)^(1/3) ;  % see Keating/Scott 99
# % OptimalBinWidth ist eine optimale Breite des einzelen Bins (Stuetzstellenabstand)
# % fuer die Optimale Anzahl von Bins gelte: 80% der Bins in den zentralen 80% der Daten
# if OptimalBinWidth>0
#     OptimalNrOfBins = max(ceil((nanmax(data)-nanmin(data))/OptimalBinWidth),10);
#   else
#     OptimalNrOfBins = 10;
# end;%if OptimalBinWidth>0
 #library('dbt.Statistics')

  #Anzahl vorhandene Daten
    if(is.matrix(Data)) nData <- colSums(!is.nan(Data))
    if(is.vector(Data)) nData <- sum(!is.nan(Data))
    if(nData<1){
      optNrOfBins<-0
    }else{
      
      sigma<-sd(Data,na.rm=TRUE)    
      p<-prctile(Data,c(0.25,0.75))
      interquartilRange<-p[2]-p[1]
      
      sigmaSir<-min(sigma,interquartilRange/1.349)
      optBinWidth<-3.49*sigmaSir/(nData)^(1/3)
      if(optBinWidth>0){
        optNrOfBins<-max(ceiling((max(Data,na.rm=TRUE)-min(Data,na.rm=TRUE))/optBinWidth),10)
      }else{
        optNrOfBins<-10
      }
    }                  
 return (optNrOfBins) 

 }

