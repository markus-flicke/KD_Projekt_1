boxcoxTrans = function(Data,Lambdawertebereich=seq(-5,5,0.1)){
# lambda=boxcoxTrans(Data)
# requires dbt.nanHandling	
# INPUT
# Data(1:n,1:d)           Data array, each Variable (col d) is NOT boxcox transformed, but the Box-Cox algorithm 
#						              proposes the lambda's for a transformation
#						              (for Transformation see ladder of powers, tukey 1977)
# Lambdawertebereich		  Bereich in denen ein Lambda fesucht werden soll, anzugeben mit seq
#							            Beispiel seq(-2,2,0.1)
#
# OUTPUT
# LambdaMatrix(1:d)        zugehoerige Lambda Werte, s.u.
# boxcoxncData             Objekt aus boxcoxncData (fals nur 1 variable gegeben) oder Liste aus Objekten aus boxcoxncData

# DESCRIPTION
# performs boxcox transformation :
# TransData(Data,Lambda) = (Data^Lambda -1)/Lambda , Lambda >0
# TransData(Data,Lambda) = log(Data)                 Lambda =0
# Lambda is such that it maximizes the Log-Likelihood Function (LLF) 
#if only one variable then:
#Lambda: Returns a matrix of output with the results of seven different normality tests and artifical covariate method. The first row of the matrix corresponds to the related estimates of lambda. The subsequent rows correspond to the p-values of different normality tests for each estimates of lambda.
#if more than one variable: the mean of the lambdas for every variable,
# Author: MT 2014, editiert 05/2015

# Nota Not Implemented! TransData(1:n,1:d)        BoxCox transformed Data
  
  
  
  if (allnans(Data) != 0) {
    withoutnans = removeNaN(Data)
    Data = withoutnans$cleanData
    #warning('NaNs found and for analysis removed') #Algorithms boxcoxns liefer zu viele Warnungen
    print('NaNs found and for analysis removed')
  }
  
  
  # all values have to be positive
  
  # ind=which(Data>0)
  # if(length(ind)>50){
  #   Data=Data[ind]
  # }else{ #MT: vielleicht verfaelscht das das ergebnis!
  #    minD <- min(Data)
  #    Data <- sapply(Data,function(x) x-minD+1)
  #    warning('BoxCox max not be rigth')
  # }
  
  Datasize <- nrow(Data)
  variables <- ncol(Data)
  nonCom=c()
  if (!length(variables)) {
    variables = 1 #MT: Falls statt Matrix Vektor ?bergeben wird
    Datasize = length(Data)
    requireNamespace('AID')
    Values = AID::boxcoxnc(
      Data,
      method = "all",
      lam = Lambdawertebereich,
      plotit = FALSE,
      rep = 30,
      p.method = "BY"
    )
    l = list(Values$result)
  } else{
    l=list()
    requireNamespace('AID')
    for (i in 1:variables) {
      tryCatch( {
      Values = suppressWarnings(
        AID::boxcoxnc(
          Data[, i],
          method = "all",
          lam = Lambdawertebereich,
          plotit = FALSE,
          rep = 30,
          p.method = "BY"
        )
      )
        l = c(l, list(Values$result))
        #print(l)
      },error=function(ex){
        warning(paste0('Lambda for column ',i,' could not be calculated'))
      })
    }
  }
  
  ## Erstelle aus komischen BoxCox-Format eine Uebersichtsmatrix aus Lambdas
  if(!length(variables)){
  lambdasind = seq(from = 1, to = 8 * 4, by = 4)
  LambdaMatrix = matrix(NA, 8, 1)
  
  x = list(Values$result)
  names = colnames(Values$result)
  
  for (j in 1:8) {
    LambdaMatrix[j, 1] = x[[1]][[lambdasind[j]]]
  }
  LambdaMatrix = t(LambdaMatrix)
  colnames(LambdaMatrix) = names
  
  }else{
    LambdaMatrix=c()
    for(i in 1:length(l)){
      LambdaMatrix=c(LambdaMatrix,mean(l[[i]][1,],na.rm = F))
    }
  }
  
  if(!length(variables)){
    return(list(LambdaMatrix = LambdaMatrix, boxcoxncData = Values))
  }else{
    return(list(LambdaMatrix = LambdaMatrix, boxcoxncData = l))
  }
}