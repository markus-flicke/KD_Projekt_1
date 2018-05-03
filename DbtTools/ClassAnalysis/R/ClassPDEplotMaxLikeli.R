# function [Kernels,ClassParetoDensities] = ClassPDEplotMaxLikeli(Data,Cls,ColorSequence,ColorSymbSequence,PlotLegend,MinAnzKernels,PlotNorm);
ClassPDEplotMaxLikeli <- function(Data, Cls, ColorSequence = DefaultColorSequence(), ClassNames = NULL, PlotLegend=TRUE, MinAnzKernels=0,PlotNorm=0,main='Pareto Density Estimation (PDE)', xlab='Data',ylab='ParetoDensity', xlim = NULL, ylim = NULL, ...){
# res=ClassPDEplotMaxLikeli(Data, Cls)
# PDEplot the data for allclasses, weight the Plot with 1 (= maximum likelihood)
# INPUT
# Data                 the Data to be plotted
# Cls                  vector of class identifiers can be integers or
#                     NaN's, need not be consecutive nor positive
# OPTIONAL
# ColorSequence        the sequence of colors used, if ==0 r not given: DefaultColorSequence
# ClassNames           Vector of classnames to show correct legend
# xlim                 Plotted area of the x-axis
# ylim                 Plotted area of the y-axis
# MinAnzKernels        Minimale Anzahl Kernels, wenn nicht angegeben oder MinAnzKernelss ==0 =>  MinAnzKernels=100;
# PlotLegend           ==1 (default) add a legent to plot
# PlotNorm             ==1 => plot Normal distribuion on top , ==2 = plot robust normal distribution,; default:  PlotNorm= 0
# OUTPUT
# Kernels,ClassParetoDensities         die PDEs
# ggobject                  ggplot2 plot
# 
#  library(reshape2)
#  library(ggplot2)
# author: Felix Pape
  if(MinAnzKernels <= 0) MinAnzKernels=100

  AnzData = length(Data)


  Out = Data
  NoNanInd <- which(!is.nan(Data))
  Data <- Data[NoNanInd]
  Cls <- Cls[NoNanInd]

    ClCou <- ClassCount(Cls)
  UniqueClasses = ClCou$UniqueClasses
  CountPerClass = ClCou$CountPerClass
  NrOfClasses = ClCou$NumberOfClasses
  ClassPercentages = ClCou$ClassPercentages # KlassenZaehlen 

  PDEP = ParetoDensityEstimation(Data=Data,paretoRadius=0,kernels=0,MinAnzKernels)
  Kernels = PDEP$kernels
  ParetoDensity = PDEP$paretoDensity
  ParetoRadiusGesamt = PDEP$paretoRadius

  ClassParetoDensities = Kernels * ones(length(Kernels),NrOfClasses)

  #Normaldist=list()
  Normaldist = matrix(data = 0, nrow = length(Kernels), ncol = NrOfClasses)
  for(c in 1:NrOfClasses){
    Class = UniqueClasses[c]
    ClassInd = which(Cls==Class)

    pdeVal <- ParetoDensityEstimation(Data[ClassInd], paretoRadius=ParetoRadiusGesamt, kernels=Kernels)

    Kernels = pdeVal$kernels
    ParetoDensity = pdeVal$paretoDensity

    if(is.null(dim(ClassParetoDensities))) ClassParetoDensities = ParetoDensity
    else ClassParetoDensities[,c] = ParetoDensity
    if(PlotNorm==1){
    M = mean(Data[ClassInd],na.rm=T) #% empirical Mean
    S = sd(Data[ClassInd],na.rm=T)  # empirical Sdev
    Normaldist[,c] = dnorm(Kernels,M,S) # the Gaussian with the empirical parametrers
   # plot(Kernels,Normaldist,PlotSymbolGauss)
    } #    if PlotNorm==1
    if(PlotNorm==2){
    M = dbt.Statistics::meanrobust(Data[ClassInd]) # empirical Mean
    S = dbt.Statistics::stdrobust(Data[ClassInd])  # empirical Sdev
    Normaldist[,c] = dnorm(Kernels,M,S) # the Gaussian with the empirical parametrers
    #plot(Kernels,Normaldist,PlotSymbolGauss)
    }#   if PlotNorm==2
  }

  #if(is.null(xlim))
  #  xlim=c(min(Kernels,na.rm=TRUE),max(Kernels,na.rm=TRUE))
  #if(is.null(ylim))
  #  ylim=c(0,max(ClassParetoDensities))

  xlength = abs(min(Kernels,na.rm=TRUE) - max(Kernels,na.rm=TRUE))
  ylength = max(ClassParetoDensities)
  if(is.null(ClassNames)){
    ClassNames = c(1:NrOfClasses)
    ClassNames <- paste("C", ClassNames, sep = "")
  }
  if(PlotNorm>0){
    #fuege als dataframe zusammen
    norms = data.frame(Normaldist)
    colnames(norms) <- ClassNames
    norms$kernels = Kernels
    normsm = melt(norms, id='kernels')
  }
  cpd = data.frame(ClassParetoDensities)
  colnames(cpd) <- ClassNames
  cpd$kernels = Kernels
  cpdm = melt(cpd, id="kernels")
  plt <- ggplot()
  if(PlotNorm>0){
    plt <- plt + geom_line(data = normsm, mapping = aes(x=kernels, y=value, color=variable), linetype = 1, size = 1.5)
  }
  plt <- plt + geom_line(data=cpdm, aes(x=kernels, y=value, color=variable))
  plt <- plt + ggtitle(main) +
    theme(plot.title = element_text(lineheight = .8, face="bold"))
  plt <- plt + ylab(ylab) + xlab(xlab)
  plt <- plt + labs(colour = "Classes")
  plt <- plt + coord_fixed(ratio = xlength/ylength)
  plt <- plt + scale_color_manual(values = ColorSequence)

  if(!is.null(xlim))
    plt <- plt + scale_x_continuous(limits = xlim) 
  if(!is.null(ylim))
    plt <- plt + scale_y_continuous(limits = ylim)
  plt

  invisible(list(Kernels=Kernels, ClassParetoDensities=ClassParetoDensities, ggobject=plt))
}
