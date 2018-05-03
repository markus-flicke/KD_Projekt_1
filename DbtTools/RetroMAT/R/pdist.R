pdist = function(X,method='euclidean',dim=2,outputisvector=FALSE){
Dmatrix = DistanceMatrix(X,method=method,dim=dim,outputisvector=outputisvector)
#Wrapper fuer DistanceMatrix damit analog zu matlab
  # computes the distance between objects in the data matrix, X, 
  # using the method specified by distance, which can be any of the following character strings
  #
  # INPUT
  # X[d,n]         Daten bestehend aus d Datensaetzen/Werten/Zeilen von n Vektoren/Variablen/Spalten ohne NaN
  #                Distanz wird jeweils zwischen zwei Zeilen berechnet
  #           
  # Optional         
  # method          method specified by distance string: 
  #                 'euclidean','sqEuclidean','mahalanobis','cityblock=manhatten','cosine','chebychev'=max(abs(x-y)),'jaccard','minkowski','manhattan','binary', 'canberra'=sum abs(x-y)/sum(abs(x)-abs(y)), 'maximum', 'braycur'=sum abs(x -y)/abs(x+y)
  # dim             if method="minkowski", choose p
  # outputisvector  Falls Vector der Distanzen benoetigt wird, wird Paket pracma fuer squareform geladen
  #
  # OUTPUT
  # Dmatrix       Distance-Matrix: Pairwise distance between pairs of objects oder Vektor(outputisvector=TRUE)

  # Autor: MT
  # 1. Editor:
  #
  # EXAMPLE
  # 
  # Nota  
  # needs package stats, vegan, sphet, biotools 
  return(Dmatrix)
}