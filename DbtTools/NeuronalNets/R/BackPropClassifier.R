NNClassifier = BackPropClassifier= function(Data,nn){
# Anwendung eines MLP-BP Classifiers
# NNCls= BackPropClassifier(Data,nn)
#
# INPUT
# Data(1:n,1:d)          cases in rows, variables in columns
# MLPBP                  das MLP mit Back-Propagation, s. package neuralnet
#
# OUTPUT
# NNCls[1:n]            Klassifikation des MLPBP nach dem Training
# author: MT 06/2016
  
erg <- compute(nn,Data)
#print(str(erg$net.result))
NNCls <- max.col(erg$net.result) #Indizes des Maximums der Spalten=des Maximalen Gewichts ergibt die Klassennummer

#anzData <- nrow(Data)
#Accuracy  <- sum(NNCls == Cls)/anzData

return(NNCls)
}
