qqpoissonfit <-
function(x, ux=0, ox=0, PoissonLambda, Name="Data", pstyle=1){

# function [Delta, MeanAbsDelta] = qqpoissonfit(x,ux,ox,PoissonLambda, name,ps);
# % qqpoisson(x,name,ps,filename);
# % x    zu zeichnende Vatiable
# % ux, ox im interval [ux, ox] wird eine geade interpoliert
# % name     bezeichnung der y-achse,  wenn nicht angegeben: name = 'Data'
# % PoissonLambda   Lambda parameter der Poisson Verteilung
# %      wenn nicht angegeben: wird er mit poissfit(x) geschätzt
# % name bezeichnung der x-achse
# %      wenn nicht angegeben: data
# % ps   ist der linientyp (.0x+ usw siehe funktion plot)
# %      wenn nicht angegeben:ps = 'b-'
# % 
# % siehe auch qqnormfit
# 
# % Author ALU
# 
# % NaN Behandlung
# x = removeNaN(x);
# if nargin<6, ps = 'b.-'; end
# if nargin<5, name = 'data'; end
# if nargin<4; % Parameter estimation of Lambda for the Poisson distribution
# 	PoissonLambda = poissfit(x); 
# end;
# if nargin<2; 
# 	Sdev= sqrt(PoissonLambda);
# 	ux = max(PoissonLambda - 3*Sdev,min(x)); %mean -3*s
# 	ox = min(PoissonLambda + 3*Sdev,max(x)); %mean +3*s
# end;
# Sdev= sqrt(PoissonLambda);
# 
# % Quantile rechnen
# Quantiles = (0.01:0.01:99.99)';PQ = Quantiles/100;
# px = prctile(x,Quantiles);
# ppoisson = poissinv(PQ,PoissonLambda);
# ppoisson = polyval(polyfit(PQ,ppoisson,1),PQ); % interpolation, da poissinv ruckelig !
# 
# % Ausgleichsgerade
# Ind = find((ppoisson>=ux)&(ppoisson<=ox));
# gx = ppoisson(Ind); gpy=px(Ind);
# gerade  = polyfit(gx,gpy,1);   % polynomdarstellung in Matlab 1. grade
# ygerade = polyval(gerade,gx);      % die interpolierneden punkte gemaess Gerade
# [Delta, MeanAbsDelta]=qqfit(gx,gpy,ygerade);  % die Abweichungen im Interpolationsbereich
# 
# % Zeichnen
# plot(ppoisson,px,ps,gx,ygerade,'m-');
# ylabel(name);
# xlabel(['Poisson, mean = ' num2str(PoissonLambda) ' sdev= ' num2str(Sdev)]);
# 
# 
 Data<-x
 pLambda<-PoissonLambda
 plotName<-Name
 plotChar<-pstyle
 y<-qqpoisson(x=Data, Lambda=pLambda, Name=plotName, pstyle=plotChar)
 fitLine <- lm(y~x)
 abline(fitLine, color="red") 

 }

