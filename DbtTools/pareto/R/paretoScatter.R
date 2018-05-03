paretoScatter=function(x,y,plotIt=TRUE,binsXY=c(20,20)){
	
#require('sm')
#require('matlab')
#require('graphics')

# function [AnzInBin,GridX,GridY] = ParetoScatter(x,y,PlotIt,BinsXY);
# % [AnzInBin,GridX,GridY] = ParetoScatter(x,y,PlotIt,BinsXY);
# % densities plotted corresponding to plot(x,y,'.')
# %
# % INPUT
# % x,y                     the data to be plotted as in plot(x,y,'.')
# % OPTIONAL
# % PlotIt                  ==1 (default) do the plotting
# % BinsXY                  [NbinsX Nbns] default == [ 20 20] Nr of bins in
# %                         is used otherwise it is calculated using ParetoRad(...)
# % OUTPUT
# % AnzInBin                the number of datapoints in a bin centered at(GridX,GridY)
# % GridX,GridY             coordinates of the bin centers 
# 
# % Author: A. Ultsch 2007
# if nargin < 4 ; BinsXY = [20 20]; end;
# if nargin < 3 ; PlotIt=1; end;
# 
# [AnzInBin,C] = hist3([x,y], BinsXY );      % anzahl in Bin feststellen
# CenterX= (C{1}');CenterY= (C{2}');         % Zentrumscoordinaten ermitteln
# [GridY GridX ]= meshgrid(CenterX,CenterY); % in gitterkoordinaten umsetzen

	if(any(is.na(x)|is.na(y))){
		print("Data may not contain missing values.")
		return()	
	}
	
requireNamespace('sm')
	
requireNamespace('matlab')
	e <- sm::binning(cbind(x,y),nbins=binsXY)
	nInBins <- t(e$table.freq)
	centerX <- e$midpoints[,1]
	centerY <- e$midpoints[,2]
	
	erg <- matlab::meshgrid(e$midpoints[,1],e$midpoints[,2])
	
	gridX <- erg$y
	gridY <- erg$x
	
	
	if(plotIt) {
		
		colorSequence=c( "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFEF", "#FFFFDF", "#FFFFCF", "#FFFFBF", "#FFFFAF", "#FFFF9F","#FFFF40", "#FFFF30", "#FFFF20", "#FFFF10", "#FFFF00", "#FFF900", "#FFF200", "#FFEC00", "#FFE500", "#FFDF00", "#FFD900", "#FFD200", "#FFCC00", "#FFC600", "#FFBF00", "#FFB900", "#FFB200", "#FFAC00", "#FFA600", "#FF9F00", "#F40000", "#EF0000", "#EA0000", "#E40000", "#DF0000", "#DA0000", "#D40000", "#CF0000", "#CA0000", "#C50000", "#BF0000", "#BA0000", "#B50000", "#AF0000", "#AA0000", "#A50000", "#9F0000", "#9A0000", "#950000", "#8F0000", "#8A0000", "#850000", "#800000", "#7A0000", "#750000", "#700000", "#6A0000", "#650000", "#600000", "#5A0000", "#550000", "#500000", "#4A0000", "#450000", "#400000", "#3A0000", "#350000", "#300000", "#2B0000", "#250000", "#200000", "#1B0000", "#150000", "#100000", "#0B0000", "#050000")
	
	ramp <- colorRamp(colorSequence[2:length(colorSequence)]) # compute colors 
#	tcol <- rgb(ramp(seq(0,1,length=29)),max=255) # RGB-values 
#	tcol <- c(colorSequence[1],tcol) # Concatenate first color with computed (to provide absolute zero)

		dbtContour(gridX[,1],t(gridY)[,1],nInBins,nlevels=25,col= colorSequence)
		contour(gridX[,1],t(gridY)[,1],nInBins,nlevels=25, drawlabels=F,xaxs='i',yaxs='i',add=TRUE)
		
	}


 return (list(nInBins=nInBins,gridX=gridX,gridY=gridY)) 

 }