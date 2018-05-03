ClassPlot <-  function(X,Y,Cls=X*0,Title=' ',Xlabel='X',Ylabel='Y',xlim=c(),ylim=c(),ColorSequence,ColorSymbSequence=NULL,PlotLegend=1,TilePlots=c(), add=F,...){
  # ClassPlot(X,Y,Cls)
  # plot XY data colored by Cls
  #
  # INPUT
  # X[1:n],Y[1:n]        the coordinates of the data
  # Cls[1:n]             integer vector of class identifiers 
  #
  #
  # OPTIONAL
  # Title                plot(..main=Title)
  # Xlabel,Ylabel        plot(..xlab='X',ylab='Y')
  # Xlim,Ylim            plot(..xlim=c(),ylim=c())
  # ColorSequence        the sequence of colors used by default 'grcmykb' 
  # ColorSymbSequence    the plot symbols used                            
  #                      if there are less than 7 classes only the first symbol is used
  # PlotLegend           ==1 (default) add a legent to plot
  # TilePlots            [l,c] if given a tile plot tileplot(l,c,*) for each class separately is given NOT JET IMPLEMENTED!
  # xlim,ylim            plot(..xlim=c(),ylim=c())
  # add			 should a new plot be drawn or should the points be drawn over the current plot
  
  
  # ALU 2014
  # 1.Editor: Willi Eggeling 07/2015
  # 2.Editor: FL 09/2015

  if(PlotLegend)
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  
  AnzData = length(X)
  
  if (length(Cls) < AnzData) {
    print(paste0("ClassPlot(): too few Classes", length(Cls),
                 " dummy added", AnzData))
    NewCls = ones(AnzData, 1) * max(Cls + 1)
    NewCls[1:AnzData] = Cls
    Cls = NewCls
  }
  if (length(Cls) > AnzData) {
    print("ClassPlot(): Cls too long, shortened")
    Cls = Cls[1:AnzData]
  }
  E <- NormalizeCls(Cls)
  NormalizedCls <- E$normalizedCls
  UniqueCls <- E$uniqueClasses
  AnzClasses <- E$numberOfClasses
  
  if(is.null(ColorSymbSequence))
    ColorSymbSequence <- DefaultColorSymbSequence()[1]
  
  PlotSymbol <- ColorSymbSequence
  
  DefaultColorSeq <- DefaultColorSequence()
  if (missing(ColorSequence))
    ColorSequence <- DefaultColorSeq
  AnzColors = length(ColorSequence)
  ColorNR = c(1:AnzColors)
  if (AnzClasses > AnzColors) {
    ColorNR = (c(0:AnzClasses)%%AnzColors) + 1
  }
  MinX = min(X,na.rm=T)
  MaxX = max(X,na.rm=T)
  MinY = min(Y,na.rm=T)
  MaxY = max(Y,na.rm=T)
  CornersX <- c(MinX, MinX, MaxX, MaxX)
  CornersY <- c(MinY, MaxY, MinY, MaxY)

  if(!add){
    plot(CornersX, CornersY, pch = PlotSymbol, col = "black",
	 xlab = Xlabel, ylab = Ylabel, xlim = xlim, ylim = ylim,
	 main = Title, new = TRUE)
  }
  points(CornersX, CornersY, pch = PlotSymbol, col = "white",
      xlab = Xlabel, ylab = Ylabel, xlim = xlim, ylim = ylim,
      main = Title,...)
  for (i in 1:AnzClasses) {
    C = UniqueCls[i]
    Ind = which(C == NormalizedCls)
    points(X[Ind],Y[Ind], pch = PlotSymbol,
           col = ColorSequence[ColorNR[i]], xlab = Xlabel, ylab = Ylabel,
           xlim = xlim, ylim = ylim, main = Title,...)
  }

  if (PlotLegend == 1) {
    legend("topright", inset=c(-0.2,0), legend = UniqueCls, fill = ColorSequence[ColorNR])
  }
}
