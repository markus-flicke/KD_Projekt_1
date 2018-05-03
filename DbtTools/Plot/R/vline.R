vline <- function (x, y=c(), Color='magenta', LineWidth=1,LineType='l', text=c(),...){
# vline(x, y, Color, LineWidth,,LineType,text);
# plot vertical line(s)
# INPUT
# x                     x coordinate(s) of lines
#
# OPTIONAL
# y                     2 element vector for y coordinates, default or [] => y axis, 
# Color                 color,    default is 'magenta'
# LineWidth             line width, default is  1
# LineType                
# text                  text, written at top of line
# ...                   All allowed arguments for points
  
# ALU 2014
# 1.Editor: MT 07/2015
par(new = TRUE) #Lässt in selber Figure eine neues Zeichnung zu
  
if (length(y)==2){
  Ylimits = y
}else{
  Ylimits = par("yaxp")[1:2] # Grenzen der Achsen
} #end if if (length(y)<1)

for (i in 1:length(x)){ 
  points(c(x[i],x[i]),Ylimits,type=LineType,lwd=LineWidth,col=Color,...);
  par(new = TRUE);
}# end for i

par(new = FALSE);
} # end function vline