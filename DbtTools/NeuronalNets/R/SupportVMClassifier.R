SupportVMClassifier <- function(Svm, Data){

  erg <-  predict(object=Svm, newdata=Data , decision.values = FALSE)
  return(erg)
}