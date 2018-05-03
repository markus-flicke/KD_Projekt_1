qquniform <-
function(x, Name="Data", pstyle=20, filename=""){

# function  qquniform(x,name,ps,filename);
# % qquniform(x,name,ps,filename);
# % QQ-Plot w.r.t. uniform distribution
# % x	   zu zeichnende Vatiable
# % name   bezeichnung der x-achse
# % ps     ist der linientyp
# %        wenn nicht angegeben: '.'
# % filename *.eps Datei für Plot, optional
# % 
# % siehe auch qqplot
# if nargin<2, name = 'Data'; end
# if nargin<3, ps = '.'; end
# if nargin > 3
#   if octave
#     initfileplot
#   end
# end
# % NaN Behandlung
# x = x(find(~(isnan(x))));
# P = [1:0.4:99]';
# Y = prctile(x,P); % percenile
# Range = max(x)-min(x);
# T = P/(P(end)-P(1)+1);
# X = Y(1) + Range*T;
# plot(X,Y,ps);
# ylabel(name);
# xlabel('uniform');
# 
# if nargin > 3
#   if matlab
#     initfileplot
#   end
#   move('plot.eps', filename);
# end

 x <- x[c(which(!is.nan(x)))]
 y<-qunif((1:length(x))/length(x),min=min(x),max=max(x)) 
 if(filename != "") postscript(paste(filename, ".eps", sep=""))
 qqplot(x, y, xlab='uniform', ylab=Name, main="Q/Q Plot compared to uniform distribution", pch=pstyle, col='blue')
 if(filename != "") dev.off()
 return(y)
 }

