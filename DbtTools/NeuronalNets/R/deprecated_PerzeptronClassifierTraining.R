deprecated_PerzeptronClassifierTraining <- function(Data,Cls,Epochs=1,PreTrainedPerzeptron=NULL){

  NNCls <- 0
  Accuracy <- 0
  Perzeptron <- 0
  WeightVector <-0
  Bias <-0
cls <- Cls[1]==Cls
 dat <- cbind(Data,1)
if( length(PreTrainedPerzeptron) ==0){
  ### Initialisierung
  weight <- c(w1=runif(1,-1,1),w2=runif(1,-1,1),theta=runif(1,-1,1))
}else{weight <- c(PreTrainedPerzeptron$WeightVector,PreTrainedPerzeptron$Bias)}
print(weight)



## Lernschritt

for(i in 1:Epochs){
	cl <- dat %*% weight > 0
	richtKlasse <- (cl==cls)
	index <- which(richtKlasse==FALSE)
	if (length(index)==0){
		print('Fertig!!!')
		break
	}
	fehl <- sample(index,1)

	weightAlt <- weight
	weight <- weight + sign(cls[fehl]-0.5)*dat[fehl,]

}


  WeightVector <- c(weight[c(1,2)])
  Bias <- c(weight[3])
  Perzeptron <-  weight

  return(list(NNCls=NNCls, Accuracy=Accuracy, Perzeptron=Perzeptron, WeightVector=WeightVector, Bias=Bias))
}