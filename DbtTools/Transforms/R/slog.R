slog = function(x,m=10){
# SignedLog = slog(x); # default log == log10
# SignedLog = slog(x,m);
# Logarithmus  mit Vorzeichen:
# SignedLog = sign(x) .* log(abs(x)+1) fuer LogBase =m, default m=10
# INPUT
# x           Data 
# OPTIONAL
#
# m           Basis des Log, wenn =0 oder nicht angegebenn => log10
#
# OUTPUT
# SignedLog = sign(x) .* log(abs(x)+1) fuer LogBase =m, default m=10
    
# MT 10/2016
  
  
  absX = abs(x)
  s = sign(x)
  
  if (m == 0) {
    return(s * log1p(absX))
  }
  
  if (m == 2) {
    return(s * log2(absX + 1))
  }
  
  if (m == 10) {
    return(s * log10(absX + 1))
  }
  else {
    factor = log(m)
    return(s * log1p(absX) * log(m))
    
  }
  
}
