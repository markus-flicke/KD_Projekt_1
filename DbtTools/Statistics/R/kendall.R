kendall <-function(InputDist,OutputDist,isdistance=TRUE){
  # kendall(InputDist,OutputDist)
  # Berechnet Kendalls Tau fuer z.B. ein Shepard Diagram
  #
  # INPUT
  # InputDist             Matrize der Distanzen des Eingaberaumes
  # OutputDist            Matrize der Distanzen des Ausgaberaumes 
  # 
  # Optional
  # isdistance			 Falls keine Distanz=FALSE
  # Output
  # Tau                   Kendalls Tau, nicht linearer (nur!) STATISTISCHER Zusammenhang
  # pVal                  Signifikanzlevel
  #
  # Author: MT
  if(isdistance){
  InputDist=as.matrix(InputDist)
  OutputDist=as.matrix(OutputDist)
  # # squareform erzeugt aus einer D= nxn matrix den
  # # Vektor der Distanzen aus der unteren Diagonalmatrix (ohne Nullen aus der Diagonale) 
  OD=squareform(OutputDist)
  ID=squareform(InputDist)
  p=stats::cor.test(x=ID,y=OD,method="kendall") 
  }else{p=stats::cor.test(x=InputDist,y=OutputDist,method="kendall") }
  print('two.sided Kendalls rank Correlation')
  return(list(tau=p$estimate,pVal=p$p.value,statistic=p$statistic)) #atomic vector und attr funktioniert hier irgendwie nicht, kp
}