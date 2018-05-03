dbt_scatterplot <- function(Data, Names="", cls=NULL){
  warning('If this does not work, try library(MASS) and function pairs(x=Data,labels=Names)')
  dbtMatlabColours <-
    c("blue", "red", "green", "cyan", "magenta", "yellow")
  
  newwindow = switch(Sys.info()['sysname'],Linux=grDevices::X11, Windows=grDevices::windows(), Mac=grDevices::quartz())
  
  matDim <- dim(Data)
  # data matrix rows
  matRows <- matDim[1]
  # data matrix columns
  matCols <- matDim[2]
  
  if (length(cls) == 0)
    cls <- rep(0, matRows)
  
  if (length(cls) != matRows) {
    stop('The class vector should have the same length as the number of rows of the data matrix')
  }
  else{
    dataMatrix <- cbind(Data, cls)
  }
  # vector for the classes
  clsUniques <- length(unique(cls))
  
  
  # number of Plots Per Window (device) ##################
  ppw <- 10
  if (matCols < 10)
    ppw <- matCols
  #total number of windows needed
  numOfWin <- ceiling(matCols / ppw)
  
  
  orMar <- par("mar")
  #newMar <- c(2.5, 3, 3, 1.2)
  #par(mfrow=c(ppw, ppw),            # plots per window
  #    pty="s",
  #    mar=newMar,                           # margins
  #    cex.axis=1.2-ppw*.075)            # set axis annotaions
  
  newMar <- c(1.5, 3, 2.5, 1.2)
  
  for (curWin in 0:(numOfWin - 1)) {
    lo <- 1 + curWin * ppw
    hi <- min(ppw + curWin * ppw, matCols)
    ppwCur <- ppw
    if (hi == matCols && (matCols %% ppw) != 0) {
      ppwCur <- matCols %% ppw
    }
    #tileplot(ppwCur,ppwCur)
    if (curWin > 0)
      newwindow()
    par(
      mfrow = c(ppwCur, ppwCur),
      pty = "m",
      mar = newMar,
      # margins
      cex.axis = 1.2 - ppw * .075
    )            # set axis annotaions
    
    for (i in lo:hi) {
      for (j in lo:hi) {
        #actual plotting
        if (i != j) {
          # not a diagonal
          plot(
            dataMatrix[, i],
            dataMatrix[, j],
            col = dbtMatlabColours[1],
            pch = 20,
            cex = 1.25,
            xlab = "",
            ylab = ""
          )
          if (clsUniques > 1) {
            for (k in 1:clsUniques) {
              index <- dataMatrix[, matCols + 1] == k
              index <- as.vector(index)
              partMatrix <- dataMatrix[index, ]
              curColour <-
                dbtMatlabColours[k %% length(dbtMatlabColours) + 1]
              points(
                partMatrix[, i],
                partMatrix[, j],
                type = "p",
                col = curColour,
                pch = 20,
                cex = 1.25,
                xlab = "",
                ylab = ""
              )
              
            }
          }
        }
        else{
          a <-
            histopt(Data[, i], Title = "", AxisLabs = FALSE) # plot histogram on diagonals
        }
        
        if (i == lo)
          title(Names[j], cex.main = 1.2, line = 1)
        if (j == lo)
          title(
            ylab = Names[i],
            cex.lab = 1.2,
            line = 2,
            font.lab = 2
          )
      } # end for j
    }  # end for i
  }
  
}