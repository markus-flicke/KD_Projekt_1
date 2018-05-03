qqplotfit <-
function(x, y, PlotSymbol=20, xug=NULL, xog=NULL, LineWidth=2, PointWidth=1.0, xlabel=NULL, ylabel=NULL, mtitle=NULL){
# Erstellt einen qqplot mit Ausgleichsgerade  
# qqplotfit(x,y,PlotSymbol,xug,xog,LineWidth);
# QQ-Plot mit Ausgleichsgerade
# INPUT
# x,y	    data for Q/Q plot
# OPTIONAL
# PlotSymbol        ist der linientyp      ,  wenn nicht angegeben: PlotSymbol='.'
# xug,xog           grenzen der interpolation  interpoliert wird fuer percentiles(x) in [xug,xog]
#                   default min(x),max(x)
# LineWidth         line with for Plot default =1
# PointWidth        Dicke der Punkte im QQplot, existert nicht in Matlab
# OUTPUT:
# Delta             die Differenzen zwischen Plot und Gerade an den Interpolationspunkten
# MeanAbsDelta      sum(abs(Delta))/AnzPoints;
# 
# in \dbt\Plot
# Author Herda reimplementiert aus Matlab von ALU
# Editor 2014: MT
      


 quants<-qqplot(x, y, pch=PlotSymbol, col="blue", cex=PointWidth, xlab=xlabel, ylab=ylabel, main=mtitle) #MT: na.rm=TRUE argument weglassen
 fit<-lm(quants$y~quants$x)
 summary(fit)
 abline(fit, col="red", lwd=LineWidth)

 }

