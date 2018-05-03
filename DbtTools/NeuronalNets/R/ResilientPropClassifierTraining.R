ResilientPropClassifierTraining=function(Data,Cls,HiddenNeurons=NULL, Epochs=1000000,PreTrainedNN,PlotIt=F){
# ein Klassifikator als NN mit Resilient-Propagation
# V = ResilientPropClassifierTraining(Data,Cls,HiddenNeurons, Epochs=1000000)
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
# PreTrainedN            NN which has been trained bevore
# PlotIt                 plottet layer struktur
#
# OUTPUT
# NNCls                  Klassifikation des NN nach dem Training
# Accuracy               Performanz als # richtige
# NN                     das NN mit Resilient-Propagation, s. package neuralnets
#  
# author: MT 06/16

library(neuralnet)  
anzData <- nrow(Data)
anzClass <- max(Cls)

clsBin <- matrix(1:anzClass ,ncol= anzData, nrow=anzClass )
clsBin <- t(clsBin) ==  as.vector(Cls)

clsNames <- paste('CLS',1:anzClass,sep='')
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
  if(missing(PreTrainedNN)){
    nn <- neuralnet( form , dat, hidden=HiddenNeurons, stepmax=Epochs, lifesign = "full", linear.output = F)
  } else {
    nn <- neuralnet( form , dat, hidden= HiddenNeurons, stepmax=Epochs, lifesign = "full", linear.output = F,
                     startweights=PreTrainedNN )
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

