`ASpheres` <- function(data,defined=NULL, attribute =NULL){

# function [RadiusA,MinParetoDichte,AnzInKugeln,AnzMitEigenschaft] = AKugeln(Data,defined,Eigenschaft);
# % Berechnug der ParetoWerte und -Dichten
# %
# % INPUT
# % Data(d,n)                Datenvektoren in den d Zeilen der Dimension n
# % defined(n)               Binaervektor: 1 = die entsprechende Variable 
# %                          geht in die Berechnung der Distanz ein
# % Eigenschaft(d)           Eigenschaft(i)==1 => Punkt wird mitgezaehlt in AnzMitEigenschaft
# %                          kann wegfallen, dann werden alle gezaehlt
# %                          d.h. AnzInKugeln==AnzMitEigenschaft
# %
# % OUTPUT
# % RadiusA                  Pareto Radius der A-Kugeln
# % MinParetoDichte          Diese Dichte wird mindestens fuer eine Pareto-Kugel verlangt
# % AnzInKugeln(d)           Anzahl der Punkte, welche in der jeweiligen Kugel liegen 
# % AnzMitEigenschaft        Anzahl der Punkte mit Eigenschaft,
# %                          welche in der jeweiligen Kugel liegen 
# 


	if(is.null(defined))
		defined<-rep(TRUE,ncol(data))
		
	if(is.null(attribute))
		attribute <-rep(TRUE,nrow(data),ncol=1)
	
		
# % Koordinaten des Punktes mit mininalen Unrealisiertem Potential (_->Ultsch2001)
# UXPercentile=18;
# UYPercentile=88;
	uxPercentile <- 18
	uyPercentile <- 88
# 
# [AnzData,AnzVariables] =size(Data);

	data <- as.matrix(data)
	nData <- nrow(data)
	nVar <- ncol(data)
# 
# if nargin<3, Eigenschaft = ones(AnzData,1); end;
# [z,s] = size(Eigenschaft);

	attribute <- as.matrix(attribute)*1

	nRow <- nrow(attribute)
	nCol <- ncol(attribute)
	
	if(nRow>nCol)
		attribute <- t(attribute)
# if z>s, Eigenschaft = Eigenschaft' ;end; % Eigenschaft ist jetzt ein Zeilenvektor
# 
# % Berechnung der Distanzen und Elimination der Eigendistanz (=0)

######### <- defined fehlt	

	dd <- as.matrix(dist(data[,defined]))

	d <- triuvec(dd)
	

	sdist <- sort(na.last=T,d)
	sdl <- length(sdist)
	nzdist <- sdist[nData+1:sdl]

# dd = distances(Data,defined) ;    % Matrix der quadrierten euklidschen distanzen
# dist = triuvec(dd);  % anneinandergereiht als vektor eischliesslich diagonalen (dist=0)
# % Elimination der Diagonalen (selbstdistanz = 0
# sdist = sort(na.last=T,dist);
# sdl = length(sdist);
# nzdist = sdist(AnzData+1:sdl); % jetzt ohne nullen
# 

	pzt <- percentiles(nzdist)
	radiusA <- pzt[uxPercentile]
# %auswahl der A-Distanz: radiusA
# pzt = percentiles(nzdist);
# RadiusA= pzt(UXPercentile);
# 

	inSphereA <- dd < radiusA
	nInSphere <- colSums(inSphereA)
	
	
# % Alle Abst‰nde kleiner als radiusA
# InKugelA = dd<RadiusA; 
# AnzInKugeln = sum(InKugelA);
# AnzMitEigenschaft = Eigenschaft * InKugelA;

	nWithAttribute <- as.vector(attribute %*% (inSphereA*1))
		
	pzt <- percentiles(nInSphere)
# 	
	minParetoDensity <- pzt[uyPercentile]
# % Paretokriterium fuer Zielgroesse
# pzt = percentiles(AnzInKugeln);
# MinParetoDichte = pzt(UYPercentile);

 return (list(radiusA=radiusA,minParetoDensity=minParetoDensity,nInSphere=nInSphere,nWithAttribute=nWithAttribute)) 

 }

