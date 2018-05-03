`PDEscatterApprox` <- function(x,y,paretoRadius=0,drawTopView=TRUE,nrOfContourLines=20,dataPointLineSpec='b',nInPspheres=NaN){

# function [AnzInPspheres,ParetoRadius] = PDEscatterApprox(x,y,ParetoRadius,DrawTopView,NrOfContourLines,DataPointLineSpec,AnzInPspheres);
# % schnelle Approximation fuer PDEScatter
# % [AnzInPspheres,ParetoRadius] = PDEscatter(x,y,ParetoRadius,DrawTopView,NrOfContourLines,DataPointLineSpec,AnzInPspheres);
# % plot the PDE on top of a scatterplot
# %
# % INPUT
# % x(1:n)          data in x dimension
# % y(1:n)          data in y dimension
# % OPTIONAL 
# % ParetoRadius            the Pareto Radius; if ==0 or not given: ParetoRadius = ParetoRad([x,y])
# % DrawTopView             ==1 means contur is drawn(default), otherwise a 3D plot is drawn 
# % NrOfContourLines        number of contour lines to be drawn
# % DataPointLineSpec       LineSpec of data points see LineSpec for documentation; if DataPointLineSpec == 'xx' data is not drawn
# % AnzInPspheres(1:n)      if the nr of points inside Pareto Spheres is known this is used as z values for the plot
# % OUTPUT
# % AnzInPspheres           the number of Points inside the ParetoSphere around each point of [x,y]
# %
# % Author ALU 2004
# % in /dbt/Pareto/
# 
# 
# % Methode: benutzt hist2tiles(...) um schnell 2D histogramme zu erzeugen
# % dann werden 8 schifted histogramme gemittelt
# 
# [AnzZeilen,AnzSpalten] = size(x); if AnzSpalten> AnzZeilen, x = x'; y = y'; end;% Make Data a column vector
# 
# % remove NaN from Data

	noNaNInd <- !is.nan(x)&!is.nan(y)
	x <- x[noNaNInd]
	y <- y[noNaNInd]
	
# NoNaNInd = find(~isnan(x) & ~isnan(y)) ; % index of not NaN
# x = x(NoNaNInd); y = y(NoNaNInd);        % remove NaN
# 


	if(paretoRadius==0){
		xBinWidth <- paretoRadius1D(x,1000,FALSE)
		yBinWidth <- paretoRadius1D(y,1000,FALSE)
		paretoRadius <- sqrt(xBinWidth^2+yBinWidth^2)  
	} else {
		xBinWidth <- paretoRadius
		yBinWidth <- ParetoRadius
	}
	
# if (nargin <3) | (ParetoRadius <= 0) % ParetoRadius not given
#     XBinWidth =ParetoRadius1D(x,1000,0);
#     YBinWidth =ParetoRadius1D(y,1000,0); 
#     ParetoRadius = sqrt(XBinWidth^2+YBinWidth^2);  
# else % ParetoRadius given
#     XBinWidth =ParetoRadius;
#     YBinWidth =ParetoRadius;
# end; % Pareto radius is calculated


# if nargin <4,   DrawTopView = 1;        end; % Default = contourplot
# if nargin <5,   NrOfContourLines=20;    end ; % set default NrOfContourLines
# if nargin <6,   DataPointLineSpec='b.'; end ; % set default NrOfContourLines

	if(is.nan(nInPspheres)){
		xMin <- min(x)
		xMax <- max(x)
		yMin <- min(y)
		yMax <- max(y)	
		
		xedge1 <- seq(xMin,(xMax+xBinWidth),xBinWidth)
		yedge1 <- seq(yMin,(yMax+yBinWidth),yBinWidth)
		xedge2 <- xedge1-0.5*xBinWidth;	
		yedge2 <- yedge1 -0.5*yBinWidth;
		xedge3 <- xedge1-0.3*xBinWidth;
		yedge3 <- yedge1 -0.3*yBinWidth;
			
		n1 <-  miniHist2Tiles(x, y, xedge1, yedge1)
		n2 <-  miniHist2Tiles(x, y, xedge2, yedge2)
		n3 <-  miniHist2Tiles(x, y, xedge1, yedge2)
		n4 <-  miniHist2Tiles(x, y, xedge2, yedge1)
		n5 <-  miniHist2Tiles(x, y, xedge1, yedge3)
		n6 <-  miniHist2Tiles(x, y, xedge3, yedge1)
		n7 <-  miniHist2Tiles(x, y, xedge2, yedge3)
		n8 <-  miniHist2Tiles(x, y, xedge3, yedge3)

		nInPspheres <- (n1+n2+n3+n4+n5+n6+n7+n8)/8
				
		
	}


# if nargin  <7  % calculate AnzInPspheres as shifted Histograms
#     Xmin = min(x); Xmax = max(x);
#     Ymin = min(y); Ymax = max(y);
# 
#     xedge1 = [Xmin:XBinWidth:Xmax+XBinWidth];
#     yedge1 = [Ymin:YBinWidth:Ymax+YBinWidth];
#     xedge2 = xedge1-0.5*XBinWidth;
#     yedge2 = yedge1 -0.5*YBinWidth;
#     xedge3 = xedge1-0.3*XBinWidth;
#     yedge3 = yedge1 -0.3*YBinWidth;
#     [NrInTiles, xedges, yedges,xBinNr,yBinNr,AnzInTile1] = hist2tiles(x, y, xedge1, yedge1);
#     [NrInTiles, xedges, yedges,xBinNr,yBinNr,AnzInTile2] = hist2tiles(x, y, xedge2, yedge2);
#     [NrInTiles, xedges, yedges,xBinNr,yBinNr,AnzInTile3] = hist2tiles(x, y, xedge1, yedge2);
#     [NrInTiles, xedges, yedges,xBinNr,yBinNr,AnzInTile4] = hist2tiles(x, y, xedge2, yedge1);
#     [NrInTiles, xedges, yedges,xBinNr,yBinNr,AnzInTile5] = hist2tiles(x, y, xedge1, yedge3);
#     [NrInTiles, xedges, yedges,xBinNr,yBinNr,AnzInTile6] = hist2tiles(x, y, xedge3, yedge1);
#     [NrInTiles, xedges, yedges,xBinNr,yBinNr,AnzInTile7] = hist2tiles(x, y, xedge2, yedge3);
#     [NrInTiles, xedges, yedges,xBinNr,yBinNr,AnzInTile8] = hist2tiles(x, y, xedge3, yedge3);
#     AnzInPspheres = (AnzInTile1+AnzInTile2+AnzInTile3+AnzInTile4+AnzInTile5+AnzInTile6+AnzInTile7+AnzInTile8)/8;
# end;
# 



# PlotHandle = zplot(x,y,AnzInPspheres,DrawTopView,NrOfContourLines,DataPointLineSpec);
# title(['PDE-scatterplot, r = ' num2str(ParetoRadius)]);

 #####################
 # zplot-Aufruf fehlt
 #####################

 }

 miniHist2Tiles <- function(x, y, xedge, yedge){
 	
 	#see if range fits, extent if necessary
	if(min(x)<xedge[1])	
		xedge <- rbind(min(x),xedge)
	if(max(x)>xedge[length(xedge)])	
		xedge <- rbind(xedge,max(x))

	#see if range fits, extent if necessary
	if(min(y)<yedge[1])	
		yedge <- rbind(min(y),yedge)
	if(max(y)>yedge[length(yedge)])	
		yedge <- rbind(yedge,max(y))


	e <- hist(x,xedge,plot=FALSE)
	xBinNr <- findInterval(x,e$breaks)
	
	e <- hist(y,yedge,plot=FALSE)
	yBinNr <- findInterval(y,e$breaks)
	


	xnbin <- length(xedge)
	ynbin <- length(yedge)
	
	xyBinEdges <- c(1:(xnbin*ynbin))
	
	xyBin <- (xBinNr-1)*ynbin + yBinNr
	
	nrInTiles <- hist(xyBin,xyBinEdges,right=FALSE,plot=FALSE)$counts
options(warn=-1)	
	nrInTiles <- matrix(nrInTiles,nrow=ynbin,ncol=xnbin)
options(warn=1)	
	
	nInTile <- rep(0,length(x))
	
	for(i in 1:length(x)){
		nInTile[i] <- nrInTiles[yBinNr[i],xBinNr[i]]
		
	}

	return (nInTile)	
 	
 }
