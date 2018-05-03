#Cmeasure <- function(x,y,method = "pathlength",p=1,distance="euclidean"){
# # res <- Cmeasure(x,y)
# # Calculate the C-Measure
# # INPUT
# # x    Vektor der Punkte in Eingaberaum
# # y    Vektor der Punkte in Ausgaberaum
# # k    Anzahl der Nachbarn in der Naehe
# # method   es wird nur Minimal Pathlength und Minimal Wiring implementiert
# # p     fuer Minimal Wiring 'wiring', Ausgaberaum, default=1
# # distance   Distanzmass in Nachbarnschaftberechnung
# # OUTPUT
# # cmeasure    the calculated c measure
  
  # # x    MATRIX der Distanzen der Punkte in Eingaberaum
  # # y    MATRIX der Distanzen der in Ausgaberaum
# MT: meiner Meinung nach fladch implementiert, richtige Version anbei:
#interpretation: da disssimilarity fuer cmeasure statt similarity, folgt je kleiner der wert desto besser

Cmeasure <- function(x, y, k = 1) {
  #k>1 nicht in papern definiert!
  #requireNamespace("Distances")
  requireNamespace("GraphAlgorithms")
  InputD = as.matrix(dist(x))
  OutputD = as.matrix(dist(y))
  spath = GraphAlgorithms::KNNGraph(OutputD, k = k)
  swiring = GraphAlgorithms::KNNGraph(InputD, k = k)
  return(c(
    MinimalPathlength = sum(InputD * spath),
    MinimWiring = sum(OutputD * swiring)
  ))
  
# # author: Becker, Seibert,Roth,Zhang 07/2015  
#   
#   
#   #Hilfsfunktion: bestimme, ob zwei Punkte Nachbarn sind
# #Es wird die Indizes von nearest neighbour zurueckliefert
# #x: Daten als Zeilevektor eingeben 
#   neighbor<- function (x, distance = "euclidean"){
#   
#   #Datengroesse bestimmen
#   n<-nrow(x)
#   
#   mat<-rep(0,n)
#   
#   #gesucht wird nur "nearest neighbors" knn=1
#   for (i in 1:n) {
#     mat[i]<-nearest(x,i)
#   }
# 
#   return (mat)
# 
# }
# 
#   #Pruefe, ob die Abbildung bijektiv ist
#   if (nrow(x)!=nrow(y)){
#     return("Es muss eine bijektive Abbildung zwischen Eingabe- und Ausgaberaeume sein!")
#   }
#   
#   n<-nrow(x)
#   switch(method, pathlength={
#     #D: Aehnlichkeitsmass mit Distanz
#     #K: Indizes mit nearest neighbors
#     D <- DistanceMatrix(x)
#     K <- neighbor(y,distance)
#   }, wiring={
#     K <- neighbor(x,distance)
#     D <- DistanceMatrix(y)^p
#   },{
#     return("Fehler! Es wird nur Minimal Pathlength und Minimal Wiring implementiert.")
#   })
#   
# #  for (i in 1:n) {
# #    for (j in 1:n) {###beachte j<i im Formel,nach Vorlesung aber alle Indizes
# #      result<-result+D[i,j]*K[i,j]
# #    }
# #  }
#   
#   #result<-D%*%K #Matrixmultiplikation,beachte j<i im Formel
# 
#   result<-0
#   
#   for(i in 1:n){
#     result<- result+D[i,K[i]]
#   }

  #return(result)  
  
}
