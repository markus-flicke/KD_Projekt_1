BackPropClassifierTraining=function(Data,Cls,HiddenNeurons=NULL, Learningrate = 0.1, Epochs=1000000,PreTrainedMLBP,PlotIt=F){
# ein Klassifikator als MLP mit Back-Propagation
# V  = BackPropClassifierTraining(Data,Cls,HiddenNeurons,Epochs,PreTrainedMLBP)
# NNCls=V$NNCls           # Klassifikation des MLPBP nach dem Training
# Accuracy=V$Accuracy     # Performanz als # richtige
# MLPBP=V$MLPBP           # das MLP mit Back-Propagation
#  
# INPUT
# Data(1:n,1:d)          cases in rows, variables in columns
# Cls(1:n)               corresponding classification
# HiddenNeurons          Vector of number of neurons in every hidden layer e.g. 
#                         from 2. to n-1 layer (1.layer defined by number of variales)
#                       last layer defined by number of classes (detials see formula)
#  
# Epochs                 number of training epochs
#
# OPTIONAL               
# PreTrainedMLBP         MLPBP which has been trained bevore
# PlotIt                 plottet layer struktur
#
# OUTPUT
# NNCls                  Klassifikation des MLPBP nach dem Training
# Accuracy               Performanz als # richtige
# MLPBP                  das MLP mit Back-Propagation, s. package neuralnets
#  
# author: MT 06/16
# Editor: FL 11/16

library(neuralnet)  
anzData <- nrow(Data)
anzClass <- length(unique(Cls))

clsBin <- matrix(unique(Cls) ,ncol= anzData, nrow=anzClass )
clsBin <- t(clsBin) ==  as.vector(Cls)

clsNames <- paste('CLS',unique(Cls),sep='')
colnames(clsBin) <- clsNames
formel <-  paste(unlist(clsNames),collapse='+')
if(length( colnames(Data))==0){colnames(Data) <- paste('Atribut_',1:ncol(Data),sep='')}

atri <- paste(colnames(Data), collapse='+')
#formel <- paste(formel,'~.',sep='')

formel <- paste(formel,'~',atri,sep='')
form <- as.formula(formel) 
 
dat <- as.data.frame( cbind(Data,clsBin))

AccuracyLog = c()
#for(i in 1:(Epochs/100)){
  if(missing(PreTrainedMLBP)){
    nn <- neuralnet( form , dat, hidden=HiddenNeurons, stepmax=Epochs, lifesign = "full", linear.output = F,
                     algorithm = "backprop", learningrate = Learningrate)
  } else {
    nn <- neuralnet( form , dat, hidden= HiddenNeurons, stepmax=Epochs, lifesign = "full", linear.output = F,
                     algorithm="backprop", learningrate = Learningrate, startweights=PreTrainedMLBP )
  }
  
  #PreTrainedMLBP = nn$data
  #nn <- neuralnet( form, dat, hidden = HiddenNeurons, 1, startweights = PreTrainedMLBP, threshold = 10)
  
  #browser() 
  #cat(i*100, " Epochen erledigt\n")
  
  #TestCls = BackPropClassifier(Data, nn = nn)
  #AccuracyLog[i] = sum(Cls == TestCls)/length(Cls)
  
  
  #if(PlotIt){
  #  plot(AccuracyLog)
  #}
#}


if(PlotIt)
  plot(nn)

erg <- nn$net.result[[1]]

if(length(erg) >0){
  NNCls <- max.col(erg)
  Accuracy  <- sum(NNCls == Cls)/anzData
}else{
  print('no result')
  return()
}

return(list(NNCls=NNCls,Accuracy=Accuracy,nn=nn))

}

