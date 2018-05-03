qqnormfit <-
function(x, xug=-3, xog=3, pstyle=20, PlotIt=TRUE, Symbol4Gerade="red", xlab = 'Normal', ylab = 'Data',
main = NULL, ...){

# function [Delta, MeanAbsDelta] = qqnormfit(x,xug,xog,name,ps,Plot,Symbol4Gerade);
# % [Delta, MeanAbsDelta] = qqnormfit(x,xug,xog,name,ps,Plot,Symbol4Gerade);
# % QQ-Plot mit Ausgleichsgerade
# % INPUT
# % x	       zu zeichnende Variable
# % xug, xog im interval [xug,xog] wird eine geade interpoliert
# % ps       ist der linientyp      ,  wenn nicht angegeben: ps='.'
# % Plot     Gezeichnet wird nur wenn plot ungleich Null, sonst Zeichnung in datei (*.eps) 
# %          wenn nicht angegeben: Plot = 1
# % OPTIONAL
# % Symbol4Gerade    plotsymbol für Ausgleichsgerade per default = 'r-'
# % OUTPUT:
# % Delta          the differeces: yint-y
# % MeanAbsDelta   sum(abs(Delta))/AnzPoints;
# xlab			legend for x-axis
# ylab			legend for y-axis
# main			title for plot
# %       
# % siehe auch qqplot
# 
# if nargin<2 
#     xug=-3; xog=3;
# end;
# if nargin<4, name = 'Data'; end;
# if nargin<5, ps = 'b.'; end;
# if nargin<6, Plot = 1; end;
# if nargin<7, Symbol4Gerade = 'r-'; end;
# 
# 
# if xog -xug < 0.1 % fehler in eingabe
#    error('qqgnormfit: Interpolationsgrenzen xug, xog falsch ');
#    return;
# end; %if xog -xug < 0.1 % 
# 
# % NaN Behandlung
# x = x(find(~(isnan(x))));
# n = length(x);
# if n < 3
#     error('qqgleichfit: zu wenige Datenpunkte, midestens 3 erforderlich');
#    return;
# end; %if n < 3
# 
# x = sort(na.last=T,x);
# X = ((1:n)-1/2)/n;
# Y = sqrt(2)*erfinv(2*X-1);
# % berechnug der Ausgleichgeraden im intevall [ug,og]
# gind= find(Y>=xug & Y<=xog);    % Bereich festlegen
# gx = Y(gind);                   % x- Bereich der Geraden
# gy = x(gind);                   % y-Bereich der Geraden
# [z,s] =size(gx); gy = reshape(gy,z,s); % auf gleiches format bringen
# gerade = polyfit(gx,gy,1);      % polynomdarstellung in Matlab 1. grade
# yint = polyval(gerade,gx);      % die interpolierneden punkte gemaess Gerade
# [Delta, MeanAbsDelta]=qqfit(gx,gy,yint);  % die Abweichungen im Interpolationsbereich
# 
# % Zeichnen
# if Plot ~=0 % zeichnen?
#    plot(Y,x,ps); % qq plot zeichnen
#    ylabel(name);
#    xlabel('Normal');
#    hold on;
#    plot(gx,yint,Symbol4Gerade,'LineWidth',3); % gerade  einzeichnen
#    hold off;
#  end ; % if Plot ~=0 % zeichnen?

# requires(dbt.RetroMAT)

# graphical parameters
if(length(xlab) == 0){
	xlab <- ''
}
if(length(ylab) == 0){
	ylab <- ''
}
if(length(main) == 0){
	main <- ''
}

#qqnorm(x, na.rm=TRUE, main="Q/Q Plot compared to normal distribution with best-fit-line", ylab=Name, col="blue", pch=20)
qqnorm(x, col="blue", pch=20, xlab = xlab, ylab = ylab, main = main, ...) 
x <- sort(na.last=T,x)
n <- length(x)
X <- ((1:n)-1/2)/n
Y <- sqrt(2)* erfinv(2*X-1)
ind <- which(Y >= xug & Y <= xog)
gx <- Y[ind]
gy <- x[ind]

gerade <- polyfit(gx,gy,1, TRUE);      # polynomdarstellung in Matlab 1. grades (in absteigender Reihenfolge)
yint <-  polyval(gerade,gx);     # die interpolierten punkte gemaess Gerade

lines(gx,yint, col = Symbol4Gerade, lwd = 3)

 }

