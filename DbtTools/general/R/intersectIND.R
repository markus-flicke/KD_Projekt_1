intersectIND=function(vector1,vector2,fast=FALSE){
# Schnitt der der beiden Vektoren sowie die zugehoerigen Indizes um Datensaetze vergleichen zu koennen
# V <- intersectIND(vector1,vector2)
# Intersection= V$intersection
# Ind2First   = V$ind1   ; # an index such that intersection == vector1[ind1]
# Ind2Second  = V$ind2   ; # an index such that intersection == vector2[ind2]
#
# INPUT
# vector1[1:n]     numerischer Vektor
# vector[1:m]      numerischer Vektor
#
# OPTIONAL
# fast                =TRUE Bei sehr haeufiger verwendung, kann spezielles Paket geladen werden, welches eine schellere Ausfuehrung ermoeglichst
#
# OUTPUT Liste V mit
# V$intersection[1:p]      numerischer Vektor des schnittes
# V$Ind2First              an index such that intersection == vector1[ind1]
# V$Ind2First              an index such that intersection == vector2[ind2]
#
# authors: 
# 1.: 06/2015 ALU
#
  intersection= intersect(vector1,vector2)
  if(fast){ # mit den schnellen funktionen fmatch aus fastmatch arbeiten
      requireRpackage('fastmatch')
      ind1=fmatch(intersection,vector1) # positions of intersection in vector1
      ind2=fmatch(intersection,vector2) # positions of intersection in vector2
  }else{# fast=FALSE
      ind1=match(intersection,vector1) # positions of intersection in vector1
      ind2=match(intersection,vector2) # positions of intersection in vector2 
  }
  V =  list(Intersection=intersection,ind1=ind1,ind2=ind2)
  return(V)
}