hline <- function (y, x=c(), Color='magenta', LineWidth=1,LineType='l', text=c(),...){
# hline(y, x, Color,LineWidth,LineType,text);
# plot horizintal line(s)
# INPUT
# y                     y coordinate(s) of lines
#
# OPTIONAL
# x                     2 element vector for c coordinates, default or [] => x axis, 
# Color                 color,    default is 'magenta'
# LineWidth             line width, default is  1
# LineType                
# text                  text, written at top of line
# ...                   All allowed arguments for points

# ALU 2014
# 1.Editor: MT 07/2015
par(new = TRUE) #Lässt in selber Figure eine neues Zeichnung zu

if (length(x)==2){
  Xlimits = x
}else{
  Xlimits = par("xaxp")[1:2] # Grenzen der Achsen
} #end if (length(x)

for (i in 1:length(y)){ 
  points(Xlimits,c(y[i],y[i]),type=LineType,lwd=LineWidth,col=Color,...); 
  par(new = TRUE);# Lässt in selber Figure eine neues Zeichnung zu
}# end for i
par(new = FALSE);
} # end function hline 