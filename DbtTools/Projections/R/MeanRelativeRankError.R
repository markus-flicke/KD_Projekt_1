MeanRelativeRankError = function(InputDists, OutputDists,k=9){
# Berechnet den Mean RelativeRank Error
  ###INPUT
  #  InputDists         Matrix mit den Distanzen im Eingaberaum
  #  OutputDists        Matrix mit den Distanzen im Ausgaberaum
  #  k(optional)                k f?r die Bestimmung der k n?chsten NachbarnU
  #
  #
  ###OUTPUT
  #  MRRE1                      Mean Relative Rank Error 1
  #  MRRE2                      Mean Relative Rank Error 2
  #  C                          Mit 1/C wurden die Werte normiert
  #
  # Author: Florian Lerch
  
  
  kNNInput = knneighborDistances(k, InputDists)
  kNNInputIndex = kNNInput$NNind
  
  kNNOutput = knneighborDistances(k, OutputDists)
  kNNOutputIndex = kNNOutput$NNind
  
  # Anzahl der Datenpunkte
  N = nrow(InputDists)

  # berechne die R?nge im Eingaberaum
  ranksInput = matrix(0,N,N)
  for(i in 1:N) ranksInput[i,] = rank(InputDists[i,], ties.method= "first")
  ranksInput = ranksInput -1 # da sonst der Punkt selbst mit Rang 1 bewertet wird
  
  ranksOutput = matrix(0,N,N)
  for(i in 1:N) ranksOutput[i,] = rank(OutputDists[i,], ties.method= "first")
  ranksOutput = ranksOutput -1 # da sonst der Punkt selbst mit Rang 1 bewertet wird
  
  # Normierung
  C = 0
  for(l in 1:k) C = C + (abs(2*l-N-1) / l)
  C = C * N
  
  #MRRE1
  MRRE1 = 0
  for(i in 1:N){
    for(j in kNNOutputIndex[i,]){ # j iteriert durch die indizes der k n?chsten Nachbarn von i
      MRRE1 = MRRE1 + abs(ranksInput[i,j] - ranksOutput[i,j]) / ranksOutput[i,j]
    }
  }
  MRRE1 = MRRE1*(1/C)
  
  MRRE2 = 0
  for(i in 1:N){
    for(j in kNNInputIndex[i,]){
      MRRE2 = MRRE2 + abs(ranksInput[i,j] - ranksOutput[i,j])/ranksInput[i,j]
    }
  }
  MRRE2 = MRRE2*(1/C)
  
  return(list(MRRE1=MRRE1,MRRE2=MRRE2,C=C))
}


