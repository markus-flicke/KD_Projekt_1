ClassRepraesentant=function(Data,Cls,NumberOfRep=1){
# [Classes,RepInd] = ClassRepraesentant(Data,Cls,NumberOfRep) 
# Representatnten suchen: dies sind die Mitglieder in den jeweiligen Klassen, die die groessten silhoutte wert haben
#
# INPUT
# Data(d,n)                    d cases,  n variables
# Cls(d)                       Cls(i) == ClusterNumber of data(i,:)
# 
# OPTIONAL
# NumberOfRep                  number of repesentative items per class
#
# OUTPUT
# Classes                     the  class numbers in in cls
# RepInd                      Data(RepInd(i))is the repraesentant of the data points in  Classes(i)

 
  silh = Silhouette(Data, Cls, showPlot = F)
  Key = c(1:length(Cls)) #Trick von ALU abgeguckt
  # Daten zusammenfassen
  Matrix = cbind(Key, silh, Cls)
  # sort descending by Silhouette values
  ind=order(silh,decreasing = T,na.last = T)
  Matrix=Matrix[ind,]
  
#Was steck in der Cls drinne
  V = ClassCount(Cls)
  UniqueClasses = V$UniqueClasses
  CountPerClass = V$CountPerClass
  NumberOfClasses = V$NumberOfClasses
  
  Classes = c()
  RepInd  = c()
# Annahme: Matrix ist korrekt vorsortiert, so dass nur die ersten Elemente rausgenommen werden muessen  
# jetzt die ersten Representatnten in jeder Klasse suchen (1)
# beachte, es soll nichtmehr Representantgen geben wie es objekte in der Cls gibt (2)
  for (c in 1:NumberOfClasses) { #iterariere ueber klassen
    Class = UniqueClasses[c]      # waehle eine aus
    AnzInClass = CountPerClass[c]   # Anzahl danten in der klasse
    ClassInd = which(Matrix[, 3] == Class)   # (1) indezes dieser Klasse
    for (j in 1:NumberOfRep) { #Wieviele Representanten sollen gesucht werden
      if (j <= AnzInClass) {#(2)
        # e sind noch welche in der Klasse
        Classes = c(Classes, Class)
        RepInd  = c(RepInd, as.vector(Matrix[ClassInd[j], 1])) #Trick: In Key stehen die Indezes die wir suchen
      }  # end if (2)
    } # end for j
  }  # end for c
return(list(Classes=Classes,RepInd=RepInd))
}