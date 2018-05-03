nancorwithmean <- function(Data){

# function [CC] = nancorwithmean(data);
# % Pearseons Korrelationskoeffizienten ohne Berucksichtigung der NaN
# % Methode : Nan -> mean, dann Pearsonkorrelation mkit corrcoef berechnen
# % INPUT
# % data       die Datenmatix, Merkmale in Spalten, Faelle in Zeilen
# % OUTPUT
# % CC        Matrix der Korrellationen
# 
# [AnzZeilen,AnzSpalten] = size(data);
# Ergaenzt = data;  % initialisierung
# for s=1:AnzSpalten % fuer alle spalten
#     Ergaenzt(:,s) = nantomean(data(:,s)); % NaN in den Spalten durch Mittelwert ersetzen
# end; % for s=1:AnzSpalten % fuer alle spalten
# CC = corrcoef(Ergaenzt);


 x<-Data
 x<-nantomean(x)
 cormat<-cor(x)
 return (cormat) 

 }

