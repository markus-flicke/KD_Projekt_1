scatterpde <-  function(Data, Names = "", cls = NULL, ticks = TRUE) {
  dbtMatlabColours <-
    c("blue", "red", "green", "cyan", "magenta", "yellow")
  
  matDim <- dim(Data)
  # data matrix rows
  matRows <- matDim[1]
  # data matrix columns
  matCols <- matDim[2]
  if (Names != "")
    colnames(Data) <- Names
  if (length(cls) == 0)
    cls <- rep(0, matRows)
  
  if (length(cls) != matRows) {
    stop('The class vector should have the same length as the number of rows of the data matrix')
  }
  else{
    dataMatrix <- cbind(Data, cls = cls)
    dataFrame  <- data.frame(dataMatrix)
  }
  # vector for the classes
  clsUniques <- length(unique(cls))
  
  # New (ggplot) Version. No multiple window plots yet TODO: this
  plotlist = list()
  for (x in colnames(dataFrame)) {
    if (x == "cls")
      next
    plt = ggplot() + theme_void() + annotate(
      geom = "text",
      x = 0,
      y = 0,
      label = x
    ) #TODO: schöner machen!
    plotlist = append(plotlist, list(plt))
    for (y in colnames(dataFrame)) {
      if (y == "cls")
        next
      if (x == y) {
        plt = PDEplot(dataFrame[[x]], title = element_blank(), plot = F)$plot
      }
      else {
        dat = data.frame(x = dataFrame[, x], y = dataFrame[, y])
        plt = ggplot() + geom_point(data = dat, aes(
          x = x,
          y = y,
          colour = cls
        )) + theme(legend.position = "none", axis.title = element_blank())
      }
      if (!ticks)
        plt = plt + theme(
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()
        )
      plotlist = append(plotlist, list(plt))
    }
  }
  # Bottom name-row
  options(warn = -1) #ignore the warning of the next line
  plt = ggplot()
  options(warn = 0)
  plotlist = append(plotlist, list(plt))
  for (x in colnames(dataFrame)) {
    if (x == "cls")
      next
    plt = ggplot() + theme_void() + annotate(
      geom = "text",
      x = 0,
      y = 0,
      label = x
    ) #TODO: schöner machen!
    plotlist = append(plotlist, list(plt))
  }
  # requireRpackage("gridExtra")
  requireNamespace('gridExtra')
  gridExtra::grid.arrange(grobs = plotlist) # Output!
  
  # Old Version
  ## number of Plots Per Window (device) ##################
  #ppw <- 10
  #if(matCols < 10) ppw <- matCols
  ##total number of windows needed
  #numOfWin <- ceiling(matCols/ppw)
  #
  #
  #orMar <- par("mar")
  ##newMar <- c(2.5, 3, 3, 1.2)
  ##par(mfrow=c(ppw, ppw),            # plots per window
  ##    pty="s",
  ##    mar=newMar,                           # margins
  ##    cex.axis=1.2-ppw*.075)            # set axis annotaions
  #
  #newMar <- c(1.5, 3, 2.5, 1.2)
  #
  #for(curWin in 0:(numOfWin-1)){
  #
  #  lo <- 1+curWin*ppw
  #  hi <- min(ppw+curWin*ppw, matCols)
  #  ppwCur <- ppw
  #  if(hi==matCols && (matCols%%ppw)!=0){
  #    ppwCur <- matCols%%ppw
  #  }
  #  #tileplot(ppwCur,ppwCur)
  #  if(curWin > 0) windows()
  #  par(mfrow=c(ppwCur, ppwCur),
  #    pty="m",
  #    mar=newMar,                           # margins
  #    cex.axis=1.2-ppw*.075)            # set axis annotaions
  #
  #    for(i in lo:hi){
  #        for(j in lo:hi){
  #            #actual plotting
  #            if(i!=j){ # not a diagonal
  #              #plot(dataMatrix[,i],dataMatrix[,j], col=dbtMatlabColours[1], pch=20, cex=1.25, xlab="", ylab="")
  #
  #              if(clsUniques > 1){
  #                for(k in 1:clsUniques){
  #                    index <- dataMatrix[,matCols+1]==k
  #                    index <- as.vector(index)
  #                    partMatrix <- dataMatrix[index,]
  #                    curColour <- dbtMatlabColours[k%%length(dbtMatlabColours)+1]
  #                    points(partMatrix[,i], partMatrix[,j], type="p", col=curColour, pch=20, cex=1.25, xlab="", ylab="")
  #
  #                }
  #              }
  #            }
  #            else{
  #                  a<-PDEplot(Data[,i], title='', AxisLabs=FALSE) # plot histogram on diagonals
  #            }
  #
  #            if(i==lo) title(Names[j], cex.main=1.2, line=1)
  #            if(j==lo) title(ylab=Names[i], cex.lab=1.2, line=2, font.lab=2)
  #        } # end for j
  #    }  # end for i
  #}
  
}
