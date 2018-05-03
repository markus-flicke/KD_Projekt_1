ClassPlotPixMatrix <-  function(Data, Cls, LowLim=NULL, HiLim=NULL, XNames=NULL, YNames=NULL){

  # ClassPlotPixMatrix(Data,Cls,Names,LowLim,HiLim);
  # plot Data matrix as a heatmap of each row, sorted by Cls and horizontal division
  # INPUT
  # Data(1:d,1:n)          data cases in rows, variables in columns
  # Cls(1:d,1)             Class vector
  # OPTIONAL
  # Names         names of the data added at x/y -axis , Names =0 means no names
  # LowLim,HiLim  limits for the color axis
  # YNames                 Namen fuer die Y-ticks
  
#  require('fields')
#  require('reshape2')
#  require('ggplot2')
  
  # TODO: Was ist mit NAs?
  
  if(is.vector(Data)){
    Data <- as.matrix(Data)
  }

  df <- as.data.frame(Data)
  df$Cls <- Cls
  if(!is.null(XNames)){
    names(df) <- XNames
  }
  
  #Sort Data by Class 
  df <- df[with(df, order(Cls)), ]
  df$id <- seq.int(nrow(df))
  
  #Plot Data
  dfnocls <- df
  dfnocls$Cls <- NULL
  ppm <- PlotPixMatrix(dfnocls, LowLim, HiLim, XNames, YNames)

  #Define Lines between Classes
  lines <- data.frame()
  for(i in c(2:length(df[,1]))){
    if (df$Cls[i-1] != df$Cls[i])
      lines <- rbind(lines, data.frame(id = ((df$id[i-1] + df$id[i]) / 2))) # Alternativ: i - 0,5
  }
  
  #Add Class-Lines
  plot <- ppm + geom_hline(data = lines, aes(yintercept = id), size=1)
  
  plot
}