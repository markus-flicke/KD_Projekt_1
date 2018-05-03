dbt_qqnorm <- function(x, Name="Data", pstyle=20){

# function [qqx,qqy] = qqnorm(x,name,ps,filename);
# % qqnorm(x,name,ps,filename);
# % Quantile/Quantile = QQ-Plot im Vergleich. zur Normalverteilung
# % INPUT
# % x	   zu zeichnende Vatiable
# % OPTIONAL
# % Name bezeichnung der x-achse
# % ps   ist der linientyp (.0x+ usw siehe funktio nplot)
# %      wenn nicht angegeben: .
# % OUTPUT
# % valx,val$y   die Punkte des qq-plots 
# Autor: Herda

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
# qqy = sort(na.last=T,x);
# n = length(qqy);
# X = ((1:n)-1/2)/n;
# qqx = sqrt(2)*erfinv(2*X-1);
# plot(qqx,qqy,ps);
# ylabel(name);
# xlabel('Normal');
# if nargin > 3
#   if matlab
#     initfileplot
#   end
#   move('plot.eps', filename);
# end

#qqy <- sort(na.last=T,x)
#n <- length(qqy)
#X <- ((1:n)-1/2)/n
#qqx <- sqrt(2)*qnorm((2*X)/2)/sqrt(2)
#qqplot(qqx, qqy, xlab="Normal", ylab=Name, type=pstyle, col="blue")

#MT 30.04.14: Interner QQ-Plot produziert viele Warnings...
  
  x[is.infinite(x)] = NA #MT: Korrektur, bereinigung von Inf
val=suppressWarnings(qqnorm(x, xlab="Normalverteilung", ylab=Name, pch=pstyle, col="blue"))
#y <- qnorm((1:length(x))/length(x),min=min(x),max=max(x))
#qqplot(x,y,xlab='norm',ylab=Name,type=pstyle,col='blue')
#return(y)
invisible(val)#MT: Output wird nicht geprintet und nur ausgegeben, falls Vairable gesetzt
}

