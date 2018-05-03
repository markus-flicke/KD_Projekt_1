histopt <- function(Data, Title="Histogram of data", PlotIt=TRUE, AxisLabs=TRUE,xlab,ylab){
# histopt(Data);
# Histogram optimaler Binanzahl. Bins besitzen alle die gleiche Groesse.
#
# INPUT
# Data[d,1]	   		Vektor der zu zeichneten Variable
# OPTIONAL
# Title 			Titel des Plots
# PlotIt   			Bei False keine angezeigt Zeichnung
# AxisLabs      	Bei False keine Axen-label
#  
# OUTPUT
# nrOfBins			Anzahl der Bins
# nrInBins[1,d] 	Startpunkt jedes Bins als Vektor
# binMids			Mitte des Bins ??
# author MT
# Ergaenzung RG, Autor unbekannt

  Data[is.infinite(Data)] = NA #MT: Korrektur, bereinigung von Inf
  optNrOfBins<-OptNrOfBins(Data)
  optNrOfBins = min(100,optNrOfBins) #RG: Aus Matlab uebernommen
# temp<-hist(Data,breaks=optNrOfBins)
 #print(optNrOfBins[1])
 #temp <- hist(Data, breaks=optNrOfBins[1], col="blue", border="light blue", main=Title)
 
 minData <- min(Data,na.rm = TRUE)
 maxData <- max(Data,na.rm = TRUE)
 i <- maxData-minData
 optBreaks <- seq(minData, maxData, i/optNrOfBins) # bins haben alle die gleiche groesse
 temp <- hist(Data, breaks=optBreaks, plot=FALSE)
 
 #box();
 Breaks <- temp$breaks;  nB <- length(Breaks)
 y <- temp$counts;
#print(max(Data,na.rm=TRUE)*0.25)
 if(PlotIt){
   # plot(x=c(min(Data,na.rm=TRUE)+min(Data,na.rm=TRUE)*0.25,max(Data,na.rm=TRUE)+max(Data,na.rm=TRUE)*0.25), y=c(0, max(temp$counts,na.rm=TRUE)), type="n", main=Title, xlab="", ylab="")
   if(missing(xlab)){xlab='Data'}  
   if(missing(ylab)){ylab='Frequency'}  
   plot(x=c(min(Data,na.rm=TRUE),max(Data,na.rm=TRUE)), y=c(0, max(temp$counts,na.rm=TRUE)*1.2), type="n", main=Title,xaxs='i',yaxs='i',axes=FALSE,xlab=xlab, ylab='',xlim=c(min(Data,na.rm=TRUE),max(Data,na.rm=TRUE)), ylim=c(0, max(temp$counts,na.rm=TRUE)*1.2))
   par(mgp=c(2.2,0.6,0)) #Abstand: c(Titel, Label, Achse)
    rect(Breaks[-nB], 0, Breaks[-1], y, col="blue", border="light blue",xlab='',ylab=ylab,xlim=c(min(Data,na.rm=TRUE),max(Data,na.rm=TRUE)), ylim=c(0, max(temp$counts,na.rm=TRUE)*1.2))
       axis(1,col="black",las=1,xaxs='i') #x-Achse
    axis(2,col="black",las=1,yaxs='i') #y-Achse
    title(ylab=ylab)
 }
  #lines(x=a$kernels,y=rep(0,length(a$kernels)),col = "black",lwd = 1)
 invisible(list(nrOfBins=length(Breaks)-1, nrInBins=y, binMids=temp$mids)) #MT: Output wird nicht geprintet und nur ausgegeben, falls Vairable gesetzt
 }

