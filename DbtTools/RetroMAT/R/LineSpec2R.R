LineSpec2R <-
function (LineSpecString='b.')
# Uebersetzung des LineSpec strings von Mathlab zu den R typen
#
# INPUT
# LineSpecString     containing Syle and/or   Type and/or    Color 
# OUTPUT
# c(Ltype,Lpch,Lcol,Llty)   see plot for docu
#
# Style
# -    Solid line (default)
# --   Dashed line
# :    Dotted line
# -.   Dash-dot line
#
#Type
#+   Plus sign
#o   Circle 
#*   Asterisk
#.   Point (default)
#x   Cross
#'square' or s  Square
#'diamond' or d  Diamond
#^   Upward-pointing triangle
#v   Downward-pointing triangle
#>   Right-pointing triangle
#<   Left-pointing triangle
#'pentagram' or p    Five-pointed star (pentagram)
#'hexagram' or h     Six-pointed star (hexagram)
#
#Color 
#r   Red
#g   Green
#b   Blue
#c   CyanmMagenta
#y   Yellow
#k   Black
#w   White
#
{ 
# defaults
Ltype='p';Lpch='.';Lcol='blue';Llty =  1
if( length(grep( '.',LineSpecString))>0){Ltype='p' }# fi
if( length(grep( '-',LineSpecString))>0){Ltype='l' }# fi
# Style -> Llty
if( length(grep( '--',LineSpecString))>0){Llty =  2; Ltype='l' }# fi
if( length(grep( ':' ,LineSpecString))>0){Llty =  3; Ltype='l' }# fi
if( length(grep( '-.',LineSpecString))>0){Llty = 4;  Ltype='l' }# fi
if( length(grep( '=' ,LineSpecString))>0){Llty = 6;  Ltype='l' } # fi
# Type -> Llty
if( length(grep( 'o',LineSpecString))>0){Lpch =  21 }# fi
if( length(grep( 'O',LineSpecString))>0){Lpch =  21 }# fi
if( length(grep( 's',LineSpecString))>0){Lpch =  22 }# fi
if( length(grep( 'd',LineSpecString))>0){Lpch =  23 }# fi
if( length(grep( 'v',LineSpecString))>0){Lpch =  25 }# fi
#if( grep( '^',LineSpecString)==1){Lpch =  24 }# fi      Not jet implemented
#if( grep( '+',LineSpecString)==1){Lpch =  '+' }# fi     Not jet implemented
#Color ->   Lcol
if( length(grep( 'r',LineSpecString))>0){Lcol =  'red'   }# fi
if( length(grep( 'g',LineSpecString))>0){Lcol =  'green' }# fi
if( length(grep( 'b',LineSpecString))>0){Lcol =  'blue'  }# fi
if( length(grep( 'c',LineSpecString))>0){Lcol =  'cyan'  }# fi
if( length(grep( 'y',LineSpecString))>0){Lcol =  'yellow'}# fi
if( length(grep( 'k',LineSpecString))>0){Lcol =  'black' }# fi
if( length(grep( 'm',LineSpecString))>0){Lcol =  'magenta'}# fi

# alle ergebnisse zurueckgeben
return(c(Ltype,Lpch,Lcol,Llty))
}  #END FUNCTION

