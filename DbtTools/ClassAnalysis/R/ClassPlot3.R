ClassPlot3 <-  function(X,Y,Z,Cls=X*0,ColorSequence = DefaultColorSequence(),ColorSymbSequence = DefaultColorSymbSequence(),PlotLegend=1){
  # ClassPlot3(X,Y,Z,Cls)
  # plot XYZ data colored by Cls
  #
  # INPUT
  # X[1:n],Y[1:n],Z[1:n]        the coordinates of the data
  # Cls[1:n]             integer vector of class identifiers
  #
  #
  # OPTIONAL
  # ColorSequence        the sequence of colors used by default 'grcmykb'
  # ColorSymbSequence    the plot symbols used
  #                      if there are less than 7 classes only the first symbol is used
  # PlotLegend           ==1 (default) add a legent to plot

  # FL 01/2017 (ported from matlab)

  #if(PlotLegend)
  #  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

  requireNamespace("rgl")

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

  ColorClass = c()
  for(i in 1:length(unique(Cls))) ColorClass[Cls == sort(unique(Cls))[i]] = i
  rgl::plot3d(X,Y,Z, col = ColorSequence[ColorClass])
}
