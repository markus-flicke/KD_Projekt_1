PDEnorm <- function(Data,paretoRadius=NaN, ylabel='PDE', title='PDEnorm',PlotSymbolPDE='blue',PlotSymbolGauss= 'magenta'){
# plot ParetoDensityEstimation (PDE) and Gaussian with empirical Mean and  Variance
#E <-  PDEnorm(Data,ParetoRadius,ylabel, title,PlotSymbolPDE,PlotSymbolGauss);
#Kernels        <- E$kernels;
#ParetoDensity  <- E$paretoDensity;
#ParetoRadius   <- E$paretoRadius;
#
# INPUT
# Data[1:n]               Vector of Data to be plotted
# 
# OPTIONAL
# ParetoRadius            the Pareto Radius, if omitted, ==0 or ==NaN then ParetoRadius = ParetoRadius(Data);
# ylabel                  label for the y-Axis of the plot 
# title                   label for the title  of the plot 
# PlotSymbolPDE           color for plotting PDE       see Function plot(...col=PlotSymbolPDE), 'blue' if omitted
# PlotSymbolGauss         color for plotting Gaussian  see Function plot(...col=PlotSymbolPDE), 'magenta' if omitted
#  
# 
# OUTPUT a list of
# kernels            the x points of the PDE function
# paretoDensity      the PDE(x)
# paretoRadius       the ParetoRadius used for the plot
  

# uses ParetoDensityEstimation()
# uses ParetoRadius()

# ALU 2014 
# angepasst an Mdbt und Doku standards
# ergaenzt um Parameter PlotSymbolPDE,PlotSymbolGauss

if(length(paretoRadius)<1 || is.nan(paretoRadius)  || (paretoRadius==0))
		paretoRadius<-ParetoRadius(Data)

# normalform der Daten
Data <- as.matrix(Data)
nRow <- nrow(Data)
nCol <- ncol(Data)	
if(nCol>nRow){ 	Data <- t(Data)	}

# bestimmung der PDE	
pdeE <- ParetoDensityEstimation(Data,paretoRadius) # PDEstimationForGauss(Data,paretoRadius)

m <- mean(Data,na.rm=TRUE) ; # Schaetzung der Parameter der Normalverteilung
s <- sd(Data,na.rm=TRUE) ;   # Schaetzung der Parameter der Normalverteilung

Normaldist <- dnorm(pdeE$kernels,m,s);  # R funktion fuer N(m,s)

lengthD <- length(pdeE$paretoDensity)
	
	if(ylabel=='default')
		ylabel <- paste('bl = PDE, mg = N(',round(m,2),',',round(s,2),')',sep='')
	if(title=='default')
		title <- 'ParetoDensityEstimation(PDE) and N(m,s)'
  # zeichnen der PDE und der geschaetzten Normalverteilung
	plot(pdeE$kernels,pdeE$paretoDensity,type='l',col=PlotSymbolPDE,xlab='Data',ylab=ylabel,main=title)
	points(pdeE$kernels,Normaldist,type='l',col=PlotSymbolGauss)
	#legend('topright', c("PDE", "N(m,s)"),fill=c(PlotSymbolPDE,PlotSymbolGauss))

  invisible(list(kernels=pdeE$kernels,paretoDensity=pdeE$paretoDensity,paretoRadius=pdeE$paretoRadius))

}
