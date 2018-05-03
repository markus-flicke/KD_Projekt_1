plotyline <-
function(Xpoints, PlotSymbol="p"){

# TODO PlotSymbol

# function plotyline(Xpoints,PlotSymbol);
# % plotyline(Xpoints,PlotSymbol);
# % zeichnet im gegenwaertigen plot y-linien ein
# %
# % INPUT
# % Xpoints     x-werte der zu zeichnenden geraden
# % OPTIONAL
# % PlotSymbol  siehe plot 'r-' by default
# 
# if (nargin<2) ;           PlotSymbol ='r-'; end; % default achsenfarbe
# 
# ax = axis;
# ymax = ax(4);
# if length(Xpoints)>1
#    for i=1:length(Xpoints)
#     hold on;plot([Xpoints(i) Xpoints(i)]', [0 ymax]',PlotSymbol);
#    end;
# else %length(Xpoints)==1
#     hold on;plot([Xpoints Xpoints]', [0 ymax]',PlotSymbol);
# end; %if length(Xpoints)>1
# hold off;

# return ("The function plotyline is not implemented") 

abline( v=Xpoints, col="magenta" )

 }

