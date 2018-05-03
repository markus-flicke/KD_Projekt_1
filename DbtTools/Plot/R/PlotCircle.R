PlotCircle <-
function(Center, Radius, PlotSymbol="p"){

# function CircleSpline  = PlotCircle(Center,Radius,PlotSymbol);
# %CircleSpline  =PlotCircle(Center,Radius,PlotSymbol);
# % zeichnet im gegenwaertigen plot einen Kreis ein
# %
# % INPUT
# % Center= [Mx,My]
# % Radius     of the circle to be plotted
# % OPTIONAL
# % PlotSymbol  siehe plot 'r-' by default
# 
# if (nargin<3) ;           PlotSymbol ='b-'; end; % default PlotFarbe
# CircleSpline = rsmak('circle',Radius,Center);
# hold on;
# fnplt(CircleSpline,PlotSymbol );
# hold off;

if(dev.cur() == 1){ app=FALSE }
else{ app=TRUE }
symbols(x=Center[1], y=Center[2], circles=c(Radius), add=app, col="blue")

 }

