qquniformfit <-
function(x, FitSymbol="p", pstyle=1, xug=0,xog=0, Name="Data", Plot=1, filename=""){

# function [Delta MeanAbsDelta] = qquniformfit(x,FitSymbol,ps,xug,xog,name,Plot,filename);
# % [Delta MeanAbsDelta] = qquniformfit(x,xug,xog,name,ps,Plot,filename);
# % QQ-Plot vs Gleichverteilung mit Ausgleichsgerade
# % INPUT
# % x	        zu zeichnende Vatiable
# % OPTIONAL
# % FitSymbol plotsymbol für die Interpolationsgerade
# % xug, xog  im interval [xug,xog] wird eine geade interpoliert
# % name      bezeichnung der y-achse,  wenn nicht angegeben: name = 'Data'
# % ps        ist der linientyp      ,  wenn nicht angegeben: ps='.'
# % Plot      Gezeichnet wird nur wenn plot ungleich Null, sonst Zeichnung in datei (*.eps)
# %           wenn nicht angegeben: Plot = 1
# % filename  *.eps Datei für Plot, optional
# % OUTPUT:
# % Delta            die Differenzen zwischen Plot und Gerade an den Interpolationspunkten
# % MeanAbsDelta     sum(abs(Delta))/AnzPoints;
# %       
# % in \dbt\Plot
# 
# % ALU 2002 
# %       
# if nargin<2  FitSymbol = 'r-'; end;
# if nargin<3, ps = 'b.'; end;
# if nargin<4  xug=nanmin(x); xog=nanmax(x);end;
# if nargin<6, name = 'Data'; end
# if nargin<7, Plot = 1; end;
# 
# if nargin > 7
#   if octave
#     initfileplot
#   end
# end
# 
# if xog -xug < 0.1 % fehler in eingabe
#    error(['qquniformfit: Interpolationsgrenzen falsch  xug: ' num2str(xug) ' xog:' num2str(xog)] );
#    return;
# end; %if xog -xug < 0.1 % 
# 
# % NaN Behandlung
# x = x(find(~(isnan(x))));
# 
# % Zeichnen
# if Plot ~=0 % zeichnen?
# 	P = [1:0.4:99]';
# 	Y = prctile(x,P) % percenile
# 	Range = max(x)-min(x);
# 	T = P/(P(end)-P(1)+1);
# 	X = Y(1) + Range*T;
# 	Ind = find((X>=xug)& (X<=xog));
# 	Xint =X(Ind); Yint = Y(Ind);
# 	Gerade = polyval(polyfit(Xint,Yint,1),Xint);
# 	Delta = Yint-Gerade ; MeanAbsDeltan=nanmean(abs(Delta));
# 	plot(Xint,Gerade,FitSymbol,X,Y,ps);
# 	ylabel(name);
# 	xlabel('uniform');
# end ; % if Plot ~=0 % zeichnen?
# 
# if nargin > 7
#   if matlab
#     initfileplot
#   end
#   move('plot.eps', filename);
# end

  if(filename != "") postscript(paste(filename, ".eps", sep=""))   
  x <- sort(na.last=T,x) 
  y<-qquniform(x, Name, pstyle)
  z<-lm(y~x)
  abline(z, col="red", lwd=2)
  if(filename != "") dev.off()
 }

