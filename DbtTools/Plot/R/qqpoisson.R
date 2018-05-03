qqpoisson <-
function(x, Lambda, Name="Data", pstyle=1){

# function qqpoisson(x,PoissonLambda, name,ps);
# % qqpoisson(x,name,ps,filename);
# % x    zu zeichnende Vatiable
# % PoissonLambda   Lambda parameter der Poisson Verteilung
# %      wenn nicht angegeben: wird er mit poissfit(x) geschätzt
# % name bezeichnung der x-achse
# %      wenn nicht angegeben: data
# % ps   ist der linientyp (.0x+ usw siehe funktion plot)
# %      wenn nicht angegeben:ps = 'b-'
# % 
# % siehe auch qqplot
# 
# % Author ALU
# 
# % NaN Behandlung
# x = removeNaN(x);
# if nargin<2, PoissonLambda = poissfit(x); end; % Parameter estimation of Lambda for the Poisson distribution
# if nargin<3, name = 'data'; end
# if nargin<4, ps = 'b.-'; end
# 
# [z,s] = size(x);
# 
# Quantiles = (0.01:0.01:99.99)'; PQ = Quantiles/100;
# px = prctile(x,Quantiles);
# ppoisson = poissinv(PQ,PoissonLambda);
# ppoisson = polyval(polyfit(PQ,ppoisson,1),PQ); % interpolation, da poissinv ruckelig !
# plot(ppoisson,px,ps);
# ylabel(name);
# xlabel(['Poisson, lambda = ' num2str(PoissonLambda)]);
# 
# 

 y<-qpois(x,Lambda)
 qqplot(x,y,xlab='Poisson',ylab=Name,pch=pstyle, type="p")
 return(y) 

 }

