qqnormALU <-
function (x,Ylabel=' ',PlotSymbol='.')
# Quantile/Quantile = QQ-Plot im Vergleich. zur Normalverteilung
# INPUT
# x	   zu zeichnende Vatiable
# OPTIONAL
# Ylabel bezeichnung der Datem-achse
# PlotSymbol   ist der linientyp (.0x+ usw siehe funktio nplot)
#      wenn nicht angegeben: .
# OUTPUT
# qqx,qqy   die Punkte des qq-plots 
{
# x = x(find(~(isnan(x))));       # nan entfernen
qqy = sort(na.last=T,x)
n = length(qqy)
X = ((1:n)-1/2)/n; X
qqx = sqrt(2)*dnorm(2*X-1); 
qqx= 2 * pnorm(x * sqrt(2)) - 1
qqx = sqrt(2)*erfinv(2*X-1);


dbtplot(qqx,qqy,PlotSymbol);
xlabel('Norm');
ylabel(Ylabel);

}  #END FUNCTION

