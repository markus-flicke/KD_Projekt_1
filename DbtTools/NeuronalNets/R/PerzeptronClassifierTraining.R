PerzeptronClassifierTraining=function(Data,Cls,Epochs=1000000,PreTrained,PlotIt=F){
# Training fuer ein neuronales Netz aus Perzeptronen
# V  = PerzeptronClassifierTraining(Data,Cls,Epochs,PreTrained)
# NNCls=V$NNCls           # Klassifikation der Daten nach dem Training
# Accuracy=V$Accuracy     # Performanz als # richtige
# nn = V$nn               # das eigentliche neuronale Netz
#  
# INPUT
# Data(1:n,1:d)          cases in rows, variables in columns
# Cls(1:n)               corresponding classification
#  
# Epochs                 number of training epochs
#
# OPTIONAL               
# PreTrained             NN which has been trained bevore
# PlotIt                 plottet layer struktur
#
# OUTPUT
# NNCls                  Klassifikation des MLPBP nach dem Training
# Accuracy               Performanz als # richtige
# nn                     das trainierte neuronale Netz
  
  library("neuralnet")
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
  
  
  
  if(missing(PreTrained)){
    nn <- neuralnet( form , dat, hidden=NULL, stepmax=Epochs, lifesign = "full")
  } else {
    nn <- neuralnet( form , dat, hidden=NULL, stepmax=Epochs, lifesign = "full", startweights=PreTrained)
  }
  
  
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