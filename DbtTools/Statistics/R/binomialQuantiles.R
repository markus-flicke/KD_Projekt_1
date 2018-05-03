binomialQuantiles <- function(ns, p, alpha=0.99){
  # [Lower, Upper] = binomialQuantiles(ns, p, alpha);
  # Grenzwerte ab denen eine zufaellige Auswahl einer Teilmenge einer bestimmte Groesse signifikant ist
  #
  # INPUT
  # ns(1:AnzData)              Anzahl Elemente in den Teilmengen
  # p                          A-priori Wahrscheinlichkeit
  # OPTIONAL
  # alpha                      Wahrscheinlichkeit, dass das Ergebnis zufaellig ist
  #                            default = 1# == 0.01 angenommen
  #
  # OUTPUT
  # lowerPercent(1:AnzData)  fuer die jeweilige Teilmengengroesse ns(i) ist ab der Unterschreitung dieser Prozentzahl das Erg. signifikant
  # upperPercent(1:AnzData)  fuer die jeweilige Teilmengengroesse ns(i) ist ab der Ueberschreitung dieser Prozentzahl das Erg. signifikant
  # LowerAnz(1:AnzData),UpperAnz(1:AnzData)  die entsprechenden Schranken fuer die Anzahlen 
  
  # nach einer Idee von F. Moerchen, September 2002
  #author  MT 08/2015 reimplementiert aus Matlab
 alpha = max(0,1-alpha)
 
 AnzData=length(ns)
 x=matrix(0,AnzData,2)
 for (i in 1:AnzData){
  x[i,] = qbinom(c((1-alpha)/2, 0.5+alpha/2),ns[i],p)/ns[i]
 }
 LowerProzent=x[,1]
 UpperProzent=x[,2]
 LowerAnz=ns/p*LowerProzent
 UpperAnz=ns/p*UpperProzent
 return (list(lowerPercent=LowerProzent,upperPercent=UpperProzent,LowerAnz=LowerAnz,UpperAnz=UpperAnz)) 

 }

