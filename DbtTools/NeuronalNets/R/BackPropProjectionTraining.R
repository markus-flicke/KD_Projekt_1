BackPropProjectionTraining=function(Data,HiddenNeurons,Epochs= 1e+05,PlotIt=F){
#   V = BackPropProjectionTraining(Data,HiddenNeurons)
#   MLPBP=V$MLPBP         #das MLP mit Back-Propagation
#   NetOutput=V$NetOutput     #Identitaet
#  ein MLPBP projektion N -> k -> N
# das Netz wird auf identitaet trainiert, dann kann die Prjektion im Flaschenhals der k-dimensionalen 
# hidden layers abgerufen werden siehe Funktion
# BackPropProjection(...)
# INPUT
# Data(1:n,1:d)          cases in rows, variables in columns
# HiddenNeurons          Vector of number of neurons in hidden layer e.g. [ 3 3]
# Epochs                 number of training epochs: the maximum steps for the training of the neural network. 
#                         Reaching this maximum leads to a stop of the neural network's training process
# OPTIONAL
# PlotIt                  Plots the reuslts if TRUE
#
# OUTPUT
# MLPBP                  das MLP mit Back-Propagation
# NetOutput              die Netzausgebe fuer alle Eingabedaten, sollte die Identitaet sein!
#                        
#author MT 06/16
  library(neuralnet)

  Data2 <- cbind(Data,Data)
  V <-  paste('V',1:ncol(Data),sep='')
  E <-  paste('E',1:ncol(Data),sep='')
  colnames(Data2) <- c(V,E)
  formel_V <-  paste(unlist(V),collapse='+')
  formel_E <-  paste(unlist(E),collapse='+')
  formel <- paste(formel_E,'~',formel_V,sep='')

  nn <- neuralnet( formel , data=Data2, hidden=HiddenNeurons,linear.output=T,stepmax=Epochs)# algorithm = 'sag')#,stepmax=100)
#, algorithm = "rprop+" # 'rprop+' and 'rprop-' refer to the resilient backpropagation with and without weight backtracking
  if(PlotIt)
    plot(nn)
    
return(list(MLPBP=nn, NetOutput=nn$response))  
  
  # ##### input
  # # Data       Datenmatrix
  # # proDim     Dimmension in die Projeziert werden soll
  # # vor        Vektor: Neuronen in den hidden Schichten vor der Projektionsschicht
  # # nach       Vektor: Neuronen in den hidden Schichten nach der Projektionsschicht
  # # threshold  Abbruchschranke
  # 
  # 
  # Data2 <- cbind(Data,Data)
  # V <-  paste('V',1:ncol(Data),sep='')
  # E <-  paste('E',1:ncol(Data),sep='')
  # colnames(Data2) <- c(V,E)
  # formel_V <-  paste(unlist(V),collapse='+')
  # formel_E <-  paste(unlist(E),collapse='+')
  # formel <- paste(formel_E,'~',formel_V,sep='')
  # 
  # nn <- neuralnet( formel , Data2, hidden=c(vor,proDim,nach),threshold)# algorithm = 'sag')#,stepmax=100)
  # net <-  c(nn,projektionsSchicht=length(vor)+1)
  # class(net) <- 'nn'
  # return(net)
  
}