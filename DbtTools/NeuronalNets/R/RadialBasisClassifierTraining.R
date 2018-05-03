RadialBasisClassifierTraining=function(Data,Cls,HiddenNeurons,Epochs=1000000,PreTrainedNN,PlotIt=F){
# ein Klassifikator mit Gauss 
# V  = RadialBasisClassifierTraining(Data,Cls,HiddenNeurons,Epochs)
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
# author: MT / FL

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


#library(inline)
#library(Rcpp)
#cpp.fun = cxxfunction(signature(data1="numeric"), 
                      # plugin="Rcpp",
                      # body='IntegerVector fun_data = data1; return(1/(1+exp(-fun_data)));')

radial <- function(k) dnorm(k)


if(missing(PreTrainedNN))
  nn <- neuralnet( form , dat, hidden= HiddenNeurons, stepmax=Epochs, act.fct = radial, lifesign = "full")
else
  nn <- neuralnet( form , dat, hidden= HiddenNeurons, stepmax=Epochs, act.fct = radial, startweights=PreTrainedNN, lifesign = "full")


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

